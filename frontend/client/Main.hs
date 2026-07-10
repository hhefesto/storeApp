-- | directo — customer storefront SPA.
--
-- All four views stay mounted and are shown/hidden by CSS so events wire
-- statically; the cart mirrors to localStorage to survive the Mercado Pago
-- redirect round-trip. Payment back-URLs deep-link via the location hash
-- (#/pago/exito?ref=…), read once at startup.
module Main where

import           Control.Monad        (forM_, void)
import           Data.Aeson           (decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Data.List            (foldl')
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe, isJust)
import qualified Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Reflex.Dom

import           Common
import           Directo.Types

main :: IO ()
main = mainWidgetWithHead headW bodyW

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Directo — Refacciones para Electrodomésticos"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport"
              <> "content" =: "width=device-width, initial-scale=1") blank
  el "style" $ text baseCss

-- ── views ───────────────────────────────────────────────────────────────

data View
  = VCatalogo
  | VCarrito
  | VCheckout
  | VConfirmacion
  | VPago Text Text   -- ^ kind (exito|error|pendiente), order ref
  deriving (Eq, Show)

type Cart = Map Int64 Int

data CartOp
  = AddOne Int64
  | SetQty Int64 Int
  | Remove Int64
  | Clear

applyOp :: CartOp -> Cart -> Cart
applyOp op cart = case op of
  AddOne pid   -> M.insertWith (+) pid 1 cart
  SetQty pid q -> if q <= 0 then M.delete pid cart else M.insert pid q cart
  Remove pid   -> M.delete pid cart
  Clear        -> M.empty

parsePayHash :: Text -> Maybe (Text, Text)
parsePayHash h =
  case T.stripPrefix "#/pago/" h of
    Nothing   -> Nothing
    Just rest ->
      let (kind, query) = T.breakOn "?" rest
          ref = fromMaybe "" (T.stripPrefix "?ref=" query)
      in if kind `elem` ["exito", "error", "pendiente"] && not (T.null ref)
           then Just (kind, ref)
           else Nothing

bodyW :: MonadWidget t m => m ()
bodyW = do
  hash <- locationHash
  let initialView = maybe VCatalogo (uncurry VPago) (parsePayHash hash)

  storedCart <- localStorageGet "directo-cart"
  let initialCart :: Cart
      initialCart = case initialView of
        -- back from a successful payment: the cart was purchased
        VPago "exito" _ -> M.empty
        _ -> fromMaybe M.empty (storedCart >>= decodeStrict . TE.encodeUtf8)

  pb <- getPostBuild
  productsE <- getAndDecode (("/api/products" :: Text) <$ pb)
  productsDyn <- holdDyn [] (fromMaybe [] <$> productsE)

  rec
    viewDyn <- holdDyn initialView navE

    cartDyn <- foldDyn (flip (foldl' (flip applyOp))) initialCart opsE

    -- persist every change
    performEvent_ $ ffor (updated cartDyn) $ \c ->
      localStorageSet "directo-cart" (TE.decodeUtf8 (BL.toStrict (encode c)))

    navTopE <- topbar cartDyn

    (navCatE, opsCatE) <- viewShell viewDyn VCatalogo (catalogo productsDyn)
    (navCarE, opsCarE) <- viewShell viewDyn VCarrito (carrito productsDyn cartDyn)
    (navChkE, opsChkE, confRefE) <- checkoutShell viewDyn productsDyn cartDyn
    confRefDyn <- holdDyn "" confRefE
    navConfE <- viewShell1 viewDyn VConfirmacion (confirmacion confRefDyn)
    navPagoE <- pagoView viewDyn initialView

    let navE = leftmost [navTopE, navCatE, navCarE, navChkE, navConfE, navPagoE]
        opsE = leftmost [opsCatE, opsCarE, opsChkE]
  pure ()

-- | Render a sub-view inside a container that is hidden unless active.
viewShell :: MonadWidget t m
          => Dynamic t View -> View -> m (Event t View, Event t [CartOp])
          -> m (Event t View, Event t [CartOp])
viewShell viewDyn me inner =
  elDynAttr "div" (shellAttrs me <$> viewDyn) inner

viewShell1 :: MonadWidget t m
           => Dynamic t View -> View -> m (Event t View)
           -> m (Event t View)
viewShell1 viewDyn me inner =
  elDynAttr "div" (shellAttrs me <$> viewDyn) inner

shellAttrs :: View -> View -> Map Text Text
shellAttrs me v =
  "class" =: "wrap" <> if v == me then mempty else "style" =: "display:none"

-- ── topbar ──────────────────────────────────────────────────────────────

topbar :: MonadWidget t m => Dynamic t Cart -> m (Event t View)
topbar cartDyn = elClass "header" "topbar" $ do
  elClass "div" "brand" $ do
    text "DIRECTO"
    el "small" $ text "Refacciones para Electrodomésticos · Querétaro"
  elClass "div" "spacer" blank
  (catBtn, _) <- elAttr' "button" ("class" =: "navbtn") $ text "Catálogo"
  (carBtn, _) <- elAttr' "button" ("class" =: "navbtn primary") $ do
    text "Carrito"
    let n = M.foldr (+) 0 <$> cartDyn
    elClass "span" "cartbadge" $ dynText (T.pack . show <$> n)
  pure $ leftmost
    [ VCatalogo <$ domEvent Click catBtn
    , VCarrito  <$ domEvent Click carBtn
    ]

-- ── catálogo ────────────────────────────────────────────────────────────

catalogo :: MonadWidget t m
         => Dynamic t [Product] -> m (Event t View, Event t [CartOp])
catalogo productsDyn = do
  (searchDyn, catDyn) <- elClass "div" "controls" $ do
    ti <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("class" =: "search" <> "placeholder" =: "Buscar por nombre, marca o SKU…")
    catD <- elClass "div" "pills" $ mdo
      allE <- pill selDyn Nothing "Todas"
      catEs <- mapM (\c -> pill selDyn (Just c) (categoryLabel c)) allCategories
      selDyn <- holdDyn Nothing (leftmost (allE : catEs))
      pure selDyn
    pure (_inputElement_value ti, catD)

  let visibleDyn = filterProducts <$> productsDyn <*> searchDyn <*> catDyn

  opsE <- elClass "div" "grid" $ do
    opsEE <- dyn $ ffor visibleDyn $ \ps -> do
      addEs <- mapM productCard ps
      pure (leftmost addEs)
    switchHold never opsEE

  pure (never, (:[]) <$> opsE)
  where
    pill catD mc label = do
      let attrs = ffor catD $ \sel ->
            "class" =: (if sel == mc then "pill on" else "pill")
      (e, _) <- elDynAttr' "button" attrs (text label)
      pure (mc <$ domEvent Click e)

filterProducts :: [Product] -> Text -> Maybe Category -> [Product]
filterProducts ps q mc =
  [ p | p <- ps
      , maybe True (== category p) mc
      , T.null ql
          || ql `T.isInfixOf` T.toLower (name p)
          || ql `T.isInfixOf` T.toLower (brand p)
          || ql `T.isInfixOf` T.toLower (sku p)
  ]
  where ql = T.toLower (T.strip q)

productCard :: MonadWidget t m => Product -> m (Event t CartOp)
productCard p = elClass "div" "card" $ do
  elClass "div" "cat" $ text (categoryLabel (category p))
  elClass "div" "nm" $ text (name p)
  elClass "div" "br" $ text (if T.null (brand p) then "—" else brand p)
  elClass "div" "pr" $ text (mxn (priceCents p))
  elClass "div" "st" $ text $
    if stock p > 0
      then "Disponible: " <> T.pack (show (stock p))
      else "Agotado"
  let attrs = if stock p > 0 then mempty else "disabled" =: "disabled"
  (btn, _) <- elAttr' "button" attrs $ text "Agregar al carrito"
  pure (AddOne (productId p) <$ domEvent Click btn)

-- ── carrito ─────────────────────────────────────────────────────────────

carrito :: MonadWidget t m
        => Dynamic t [Product] -> Dynamic t Cart
        -> m (Event t View, Event t [CartOp])
carrito productsDyn cartDyn = do
  elClass "h2" "h2" $ text "Carrito"
  let linesDyn = cartLines <$> productsDyn <*> cartDyn

  opsE <- el "table" $ do
    el "thead" $ el "tr" $
      forM_ ["Producto", "Precio", "Cantidad", "Subtotal", ""] (el "th" . text)
    opsEE <- el "tbody" $ dyn $ ffor linesDyn $ \ls ->
      if null ls
        then do
          el "tr" $ elAttr "td" ("colspan" =: "5") $
            elClass "span" "muted" $ text "Tu carrito está vacío."
          pure never
        else leftmost <$> mapM cartRow ls
    switchHold never opsEE

  let totalDyn = sum . map (\(p, q) -> priceCents p * q) <$> linesDyn
  elClass "div" "total" $ do
    text "Total: "
    dynText (mxn <$> totalDyn)

  (chkBtn, _) <- elAttr' "button" ("class" =: "cta") $ text "Proceder al pago"
  let goCheckout = gate (not . M.null <$> current cartDyn) (domEvent Click chkBtn)
  pure (VCheckout <$ goCheckout, (:[]) <$> opsE)

cartLines :: [Product] -> Cart -> [(Product, Int)]
cartLines ps cart =
  Data.Maybe.mapMaybe (\p -> (p,) <$> M.lookup (productId p) cart) ps

cartRow :: MonadWidget t m => (Product, Int) -> m (Event t CartOp)
cartRow (p, q) = el "tr" $ do
  el "td" $ text (name p)
  el "td" $ text (mxn (priceCents p))
  decE <- el "td" $ do
    (dec, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "−"
    text ("  " <> T.pack (show q) <> "  ")
    (inc, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "+"
    pure $ leftmost
      [ SetQty (productId p) (q - 1) <$ domEvent Click dec
      , SetQty (productId p) (min (stock p) (q + 1)) <$ domEvent Click inc
      ]
  el "td" $ text (mxn (priceCents p * q))
  remE <- el "td" $ do
    (rm, _) <- elAttr' "button" ("class" =: "danger") $ text "Quitar"
    pure (Remove (productId p) <$ domEvent Click rm)
  pure (leftmost [decE, remE])

-- ── checkout ────────────────────────────────────────────────────────────

checkoutShell :: MonadWidget t m
              => Dynamic t View -> Dynamic t [Product] -> Dynamic t Cart
              -> m (Event t View, Event t [CartOp], Event t Text)
checkoutShell viewDyn productsDyn cartDyn =
  elDynAttr "div" (shellAttrs VCheckout <$> viewDyn) $ do
    elClass "h2" "h2" $ text "Datos de envío"
    elClass "p" "muted" $
      text "Enviamos a todo México por DHL desde Querétaro."

    (reqDyn, validDyn) <- elClass "div" "panel" $ do
      (nameD, emailD, phoneD) <- elClass "div" "formgrid" $ do
        n <- field "Nombre completo *" "text"
        e <- field "Correo electrónico *" "email"
        p <- field "Teléfono *" "tel"
        pure (n, e, p)
      (calleD, numD, colD, cpD) <- elClass "div" "formgrid" $ do
        c  <- field "Calle *" "text"
        nu <- field "Número *" "text"
        co <- field "Colonia *" "text"
        cp <- field "Código postal *" "text"
        pure (c, nu, co, cp)
      (ciuD, estD, refD) <- elClass "div" "formgrid" $ do
        ci <- field "Ciudad *" "text"
        es <- field "Estado *" "text"
        re <- field "Referencias (opcional)" "text"
        pure (ci, es, re)
      let addrDyn = ShippingAddress
            <$> calleD <*> numD <*> colD <*> cpD <*> ciuD <*> estD
            <*> (nonEmpty <$> refD)
          itemsDyn = map (uncurry CartItem) . M.toList <$> cartDyn
          reqD = CheckoutReq
            <$> nameD <*> emailD <*> phoneD <*> addrDyn <*> itemsDyn
          validD = all (not . T.null . T.strip)
            <$> sequence [nameD, emailD, phoneD, calleD, numD, colD, cpD, ciuD, estD]
      pure (reqD, (&&) <$> validD <*> (not . M.null <$> cartDyn))

    rec
      let btnAttrsDyn = ffor ((,) <$> validDyn <*> busyDyn) $ \(ok, busy) ->
            "class" =: "cta" <> if ok && not busy then mempty else "disabled" =: "disabled"
      (payBtn, _) <- elDynAttr' "button" btnAttrsDyn $ text "Continuar al pago"
      let submitE = gate (current validDyn) (domEvent Click payBtn)
      respE <- performRequestAsync (postJsonXhr "/api/orders" <$> current reqDyn <@ submitE)
      busyDyn <- holdDyn False (leftmost [True <$ submitE, False <$ respE])

    let checkoutRespE = fmapMaybe (decodeXhr :: XhrResponse -> Maybe CheckoutResp) respE
        failedE = ffor respE $ \r -> _xhrResponse_status r >= 400
        errE = fmapMaybe (\failed -> if failed then Just () else Nothing) failedE
        redirectE  = fmapMaybe initPoint checkoutRespE
        devDoneE   = fmapMaybe
          (\cr -> if isJust (initPoint cr) then Nothing else Just (orderRef cr))
          checkoutRespE

    errDyn <- holdDyn "" $ leftmost
      [ "No se pudo registrar el pedido. Revisa los datos e inténtalo de nuevo." <$ errE
      , "" <$ submitE
      ]
    elClass "p" "error" $ dynText errDyn

    -- Mercado Pago: leave the SPA for the hosted checkout.
    performEvent_ (redirectTo <$> redirectE)

    let opsE = [Clear] <$ devDoneE
        navE = VConfirmacion <$ devDoneE
    pure (navE, opsE, devDoneE)
  where
    nonEmpty t = let s = T.strip t in if T.null s then Nothing else Just s
    field label typ = elClass "div" "field" $ do
      el "label" $ text label
      ti <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: typ)
      pure (_inputElement_value ti)

-- ── confirmación (sin pago en línea) ────────────────────────────────────

confirmacion :: MonadWidget t m => Dynamic t Text -> m (Event t View)
confirmacion refDyn = elClass "div" "panel" $ do
  elClass "h2" "h2" $ text "¡Pedido registrado!"
  el "p" $ do
    text "Tu número de pedido es "
    el "strong" $ dynText refDyn
    text "."
  elClass "p" "muted" $
    text "El pago en línea no está disponible en este entorno. Nos pondremos en contacto contigo para coordinar el pago y el envío."
  (btn, _) <- elAttr' "button" ("class" =: "cta") $ text "Volver al catálogo"
  pure (VCatalogo <$ domEvent Click btn)

-- ── resultado de pago (back-urls de Mercado Pago) ───────────────────────

pagoView :: MonadWidget t m => Dynamic t View -> View -> m (Event t View)
pagoView viewDyn initialView =
  elDynAttr "div" (attrs <$> viewDyn) $ case initialView of
    VPago kind ref -> do
      elClass "div" "panel" $ do
        elClass "h2" "h2" $ text $ case kind of
          "exito"     -> "¡Gracias por tu compra!"
          "pendiente" -> "Pago en proceso"
          _           -> "El pago no se completó"
        el "p" $ do
          text "Pedido "
          el "strong" $ text ref
        statusDyn <- pollStatus ref
        el "p" $ do
          text "Estado actual: "
          el "strong" $ dynText (maybe "consultando…" orderStatusLabel <$> statusDyn)
        elClass "p" "muted" $ text $ case kind of
          "exito" -> "Recibirás la confirmación del envío por correo."
          _       -> "Si el pago no se concreta, puedes intentarlo de nuevo desde el catálogo."
      (btn, _) <- elAttr' "button" ("class" =: "cta") $ text "Volver al catálogo"
      pure (VCatalogo <$ domEvent Click btn)
    _ -> pure never
  where
    attrs v = "class" =: "wrap" <>
      case v of
        VPago _ _ -> mempty
        _         -> "style" =: "display:none"

pollStatus :: MonadWidget t m => Text -> m (Dynamic t (Maybe OrderStatus))
pollStatus ref = do
  tickE <- tickLossyFromPostBuildTime 3
  respE <- getAndDecode (("/api/orders/" <> ref <> "/status") <$ tickE)
  holdDyn Nothing (fmap osrStatus <$> respE)
