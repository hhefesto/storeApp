-- | directo — customer storefront SPA.
--
-- An improved redesign of the legacy directo-qro.com: same pages, copy
-- and catalog, rebuilt on the "ficha de almacén" design system (see
-- Common.baseCss). Views stay mounted and are shown/hidden by CSS so
-- events wire statically; the cart mirrors to localStorage to survive
-- the Mercado Pago redirect round-trip. Deep links ride the location
-- hash (#/tienda, #/producto/<slug>, #/pago/exito?ref=…), read once at
-- startup and written on every navigation.
module Main where

import           Control.Monad        (forM_, void, when)
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
  el "title" $ text "Directo — Refacciones para Electrodomésticos en Querétaro"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport"
              <> "content" =: "width=device-width, initial-scale=1") blank
  elAttr "meta" ("name" =: "description"
              <> "content" =: "Refacciones para electrodomésticos originales a los precios más accesibles en Querétaro. Más de 43 años de experiencia.") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/static/fonts/fonts.css") blank
  el "style" $ text baseCss
  el "style" $ text clientCss

-- ── views ───────────────────────────────────────────────────────────────

data View
  = VInicio
  | VInfo Text        -- ^ one of the seven category info pages, by slug
  | VTienda (Maybe Category)
  | VProducto Text    -- ^ product slug
  | VContacto
  | VCarrito
  | VCheckout
  | VConfirmacion
  | VCuenta
  | VPago Text Text   -- ^ kind (exito|error|pendiente), order ref
  deriving (Eq, Show)

-- | The seven informational pages, copy from the legacy site.
data InfoPage = InfoPage
  { ipSlug  :: Text
  , ipNav   :: Text
  , ipTitle :: Text
  , ipParas :: [Text]
  , ipCat   :: Category
  }

infoPages :: [InfoPage]
infoPages =
  [ InfoPage "lavadoras" "Lavadoras"
      "Asegura tu compostura con refacciones para lavadoras originales"
      [ "En Directo contamos con un amplio surtido de refacciones para lavadoras, por lo que con nosotros encontrarás desde agitadores, bandas, bombas, flechas, transmisiones y poleas hasta válvulas de todas las marcas."
      , "Ya sean modelos recientes o antiguos, contamos con cualquier refacción para lavadoras, incluidas tarjetas electrónicas de todas las marcas."
      ]
      Lavadoras
  , InfoPage "secadoras" "Secadoras"
      "Óptimos resultados con nuestras refacciones para secadoras"
      [ "Si de refacciones para secadoras se trata, en Directo encontrarás cualquier tipo de banda para secadora, ignitores, espreas, motores, fieltros, deslizadores, ductos y trampas de pelusa para el cuidado de las prendas."
      , "Sea cual sea la marca de tu secadora, contamos con todas las tarjetas electrónicas, sensores y termostatos que tu equipo necesita."
      ]
      Secadoras
  , InfoPage "refrigeradores-domesticos" "Refrigeradores Domésticos"
      "Mejora su vida útil con refacciones para refrigeración de calidad"
      [ "¿Necesitas refacciones para refrigeración? Las puedes encontrar en Directo: tenemos desde tarjetas electrónicas, focos, apagadores, controles y gases refrigerantes hasta tubería de cobre, soldadura, tanques y boquillas para soldar, manifolds, motores condensadores, aspas de aluminio y plástico, válvulas de expansión, gas R-600, gas R-290, aceites y deshidratadores."
      ]
      Refrigeradores
  , InfoPage "aire-acondicionado" "Aire Acondicionado"
      "¡Refacciones para aire acondicionado originales!"
      [ "Para tu comodidad, en Directo contamos con minisplits marca Whirlpool en voltajes de 110 y 220, tanto de solo frío como de frío/calor, además de sus refacciones."
      ]
      AiresAcondicionados
  , InfoPage "extractores-de-aire" "Extractores de Aire"
      "Tenemos todo tipo de refacciones para extractores de aire"
      [ "Si de extractores de aire y sus refacciones se trata, en Directo tenemos los equipos y sus piezas originales: extractores centrífugos de 4 a 10 pulgadas, extractores axiales de 20 a 50 cm de diámetro, baleros, capacitores de arranque y trabajo, aspas y turbinas."
      ]
      Extractores
  , InfoPage "licuadoras-y-enseres-menores" "Licuadoras y Enseres Menores"
      "Conoce las refacciones para electrodomésticos originales y genéricas"
      [ "Somos especialistas en refacciones para electrodomésticos. Si te dedicas a la reparación de licuadoras y enseres menores, en Directo podrás encontrar diafragmas, cuchillas, coples, vasos (de plástico y de cristal), carbones y apagadores, entre otros."
      ]
      Licuadoras
  , InfoPage "motores-y-bombas" "Motores y Bombas"
      "Asesoría profesional en refacciones para motores y bombas"
      [ "En Directo encontrarás bombas monofásicas marca Siemens de ¼ a 3 hp y motores monofásicos de ½ a 2 hp Siemens, así como sus refacciones."
      ]
      MotoresYBombas
  ]

infoBySlug :: Text -> Maybe InfoPage
infoBySlug s = Data.Maybe.listToMaybe [ ip | ip <- infoPages, ipSlug ip == s ]

-- ── hash routing ────────────────────────────────────────────────────────

viewToHash :: View -> Text
viewToHash = \case
  VInicio       -> "/"
  VInfo s       -> "/" <> s
  VTienda mc    -> "/tienda" <> maybe "" (\c -> "?categoria=" <> T.pack (show c)) mc
  VProducto s   -> "/producto/" <> s
  VContacto     -> "/contacto"
  VCarrito      -> "/carrito"
  VCheckout     -> "/checkout"
  VConfirmacion -> "/confirmacion"
  VCuenta       -> "/cuenta"
  VPago k r     -> "/pago/" <> k <> "?ref=" <> r

hashToView :: Text -> View
hashToView h0 =
  let h = fromMaybe h0 (T.stripPrefix "#" h0)
  in case () of
    _ | Just pr <- parsePayHash h                     -> uncurry VPago pr
      | "/cuenta" `T.isPrefixOf` h                    -> VCuenta
      | h == "/carrito"                               -> VCarrito
      | h == "/contacto"                              -> VContacto
      | Just rest <- T.stripPrefix "/producto/" h     -> VProducto rest
      | "/tienda" `T.isPrefixOf` h                    ->
          let q = T.drop 1 (T.dropWhile (/= '?') h)
              mc = T.stripPrefix "categoria=" q >>= categoryFromText
          in VTienda mc
      | Just ip <- T.stripPrefix "/" h >>= infoBySlug . T.takeWhile (/= '?')
                                                      -> VInfo (ipSlug ip)
      | otherwise                                     -> VInicio

parsePayHash :: Text -> Maybe (Text, Text)
parsePayHash h =
  case T.stripPrefix "/pago/" h of
    Nothing   -> Nothing
    Just rest ->
      let (kind, query) = T.breakOn "?" rest
          ref = fromMaybe "" (T.stripPrefix "?ref=" query)
      in if kind `elem` ["exito", "error", "pendiente"] && not (T.null ref)
           then Just (kind, ref)
           else Nothing

-- ── cart ────────────────────────────────────────────────────────────────

type Cart = Map Int64 Int

data CartOp
  = AddOne Int64
  | AddQty Int64 Int
  | SetQty Int64 Int
  | Remove Int64
  | Clear

applyOp :: CartOp -> Cart -> Cart
applyOp op cart = case op of
  AddOne pid   -> M.insertWith (+) pid 1 cart
  AddQty pid q -> M.insertWith (+) pid q cart
  SetQty pid q -> if q <= 0 then M.delete pid cart else M.insert pid q cart
  Remove pid   -> M.delete pid cart
  Clear        -> M.empty

-- ── main widget ─────────────────────────────────────────────────────────

bodyW :: MonadWidget t m => m ()
bodyW = do
  hash <- locationHash
  let initialView = hashToView hash

  storedCart <- localStorageGet "directo-cart"
  let initialCart :: Cart
      initialCart = case initialView of
        -- back from a successful payment: the cart was purchased
        VPago "exito" _ -> M.empty
        _ -> fromMaybe M.empty (storedCart >>= decodeStrict . TE.encodeUtf8)

  pb <- getPostBuild
  productsE <- getAndDecode (("/api/products" :: Text) <$ pb)
  productsDyn <- holdDyn [] (fromMaybe [] <$> productsE)

  -- signed-in customer (Nothing until /api/me answers)
  meRespE <- performRequestAsync (getJson "/api/me" <$ pb)
  authCfgE <- getAndDecode (("/api/auth/config" :: Text) <$ pb)
  providersDyn <- holdDyn [] (maybe [] acProviders <$> authCfgE)
  payCfgE <- getAndDecode (("/api/payment-config" :: Text) <$ pb)
  payProvidersDyn <- holdDyn [] (maybe [] pcProviders <$> payCfgE)

  rec
    userDyn <- holdDyn Nothing $ leftmost
      [ (decodeXhr :: XhrResponse -> Maybe UserInfo) <$> meRespE
      , Nothing <$ loggedOutE
      ]

    viewDyn <- holdDyn initialView navE

    -- reflect navigation in the URL (shareable deep links)
    performEvent_ $ setLocationHash . viewToHash <$> navE

    cartDyn <- foldDyn (flip (foldl' (flip applyOp))) initialCart opsE

    -- persist every change
    performEvent_ $ ffor (updated cartDyn) $ \c ->
      localStorageSet "directo-cart" (TE.decodeUtf8 (BL.toStrict (encode c)))

    navTopE <- header cartDyn userDyn

    navIniE                     <- viewShell1 viewDyn isInicio (inicio productsDyn)
    navInfoE                    <- infoShell viewDyn productsDyn
    (navTdaE, opsTdaE)          <- tiendaShell viewDyn productsDyn
    (navPrdE, opsPrdE)          <- productoShell viewDyn productsDyn
    navCtoE                     <- viewShell1 viewDyn (== VContacto) contacto
    (navCarE, opsCarE)          <- viewShell viewDyn (== VCarrito) (carrito productsDyn cartDyn)
    (navChkE, opsChkE, confRefE) <- checkoutShell viewDyn productsDyn cartDyn userDyn payProvidersDyn
    confRefDyn <- holdDyn "" confRefE
    navConfE <- viewShell1 viewDyn (== VConfirmacion) (confirmacion confRefDyn)
    (navCtaE, loggedOutE) <- cuentaView viewDyn userDyn providersDyn
    navPagoE <- pagoView viewDyn initialView

    footNavE <- footer

    let navE = leftmost [ navTopE, navIniE, navInfoE, navTdaE, navPrdE, navCtoE
                        , navCarE, navChkE, navConfE, navCtaE, navPagoE, footNavE ]
        opsE = leftmost [opsTdaE, opsPrdE, opsCarE, opsChkE]
  pure ()
  where
    isInicio v = v == VInicio

-- | Render a sub-view inside a container hidden unless the predicate holds.
viewShell :: MonadWidget t m
          => Dynamic t View -> (View -> Bool) -> m (Event t View, Event t [CartOp])
          -> m (Event t View, Event t [CartOp])
viewShell viewDyn isMe inner =
  elDynAttr "div" (shellAttrs isMe <$> viewDyn) inner

viewShell1 :: MonadWidget t m
           => Dynamic t View -> (View -> Bool) -> m (Event t View)
           -> m (Event t View)
viewShell1 viewDyn isMe inner =
  elDynAttr "div" (shellAttrs isMe <$> viewDyn) inner

shellAttrs :: (View -> Bool) -> View -> Map Text Text
shellAttrs isMe v =
  "class" =: "vista" <> if isMe v then mempty else "style" =: "display:none"

-- ── header ──────────────────────────────────────────────────────────────

header :: MonadWidget t m
       => Dynamic t Cart -> Dynamic t (Maybe UserInfo) -> m (Event t View)
header cartDyn userDyn = elClass "header" "cabecera" $ do
  elClass "div" "cabecera-in" $ do
    (logoE, _) <- elAttr' "a" ("class" =: "logo" <> "href" =: "#/") $ do
      elClass "span" "logo-marca display" $ text "Directo"
      elClass "span" "logo-sub" $ text "Refacciones para electrodomésticos · Querétaro"

    navE <- elClass "nav" "navprincipal" $ do
      iniE <- navLink "Inicio" VInicio
      refE <- elClass "div" "menu-refacciones" $ do
        elClass "button" "navlink menu-btn" $ do
          text "Refacciones"
          elClass "span" "caret" $ text "▾"
        elClass "div" "menu-lista" $ do
          es <- mapM (\ip -> navLink (ipNav ip) (VInfo (ipSlug ip))) infoPages
          pure (leftmost es)
      tdaE <- navLink "Tienda" (VTienda Nothing)
      conE <- navLink "Contacto" VContacto
      pure (leftmost [iniE, refE, tdaE, conE])

    accE <- elClass "div" "cabecera-acciones" $ do
      elAttr "a" ("class" =: "tel mono" <> "href" =: "tel:4422154990") $
        text "442 215 4990"
      (ctaBtn, _) <- elAttr' "button" ("class" =: "navlink cuenta-btn") $
        dynText (maybe "Entrar" (T.takeWhile (/= ' ') . uiName) <$> userDyn)
      (carBtn, _) <- elAttr' "button" ("class" =: "carrito-btn") $ do
        text "Carrito"
        let n = M.foldr (+) 0 <$> cartDyn
        dyn_ $ ffor n $ \k -> when (k > 0) $
          elClass "span" "cartbadge" $ text (T.pack (show k))
      pure $ leftmost [ VCuenta <$ domEvent Click ctaBtn
                      , VCarrito <$ domEvent Click carBtn ]

    pure $ leftmost [VInicio <$ domEvent Click logoE, navE, accE]
  where
    navLink label v = do
      (e, _) <- elAttr' "button" ("class" =: "navlink") $ text label
      pure (v <$ domEvent Click e)

-- ── inicio ──────────────────────────────────────────────────────────────

inicio :: MonadWidget t m => Dynamic t [Product] -> m (Event t View)
inicio productsDyn = do
  -- hero
  heroE <- elClass "section" "hero" $ elClass "div" "hero-in" $ do
    elClass "p" "eyebrow hero-eyebrow" $ text "Desde 1983 · Querétaro, Qro."
    elClass "h1" "display hero-titulo" $
      text "Refacciones para electrodomésticos a los precios más accesibles"
    elClass "p" "hero-sub" $
      text "Piezas originales para las marcas GE, Easy, Whirlpool, Acros, LG, Daewoo, Samsung, Westinghouse, Maytag y Koblenz — con asesoría técnica en el mostrador y envíos a todo México."
    elClass "div" "hero-ctas" $ do
      (tdaBtn, _) <- elAttr' "button" ("class" =: "cta") $ text "Ver tienda"
      (conBtn, _) <- elAttr' "button" ("class" =: "cta contorno claro") $ text "Contáctanos"
      pure $ leftmost [ VTienda Nothing <$ domEvent Click tdaBtn
                      , VContacto <$ domEvent Click conBtn ]

  -- about
  elClass "section" "seccion" $ do
    elClass "p" "eyebrow" $ text "La casa"
    elClass "h2" "display seccion-titulo" $ text "Más de 43 años surtiendo el mostrador"
    elClass "div" "dos-cols" $ do
      el "div" $ do
        el "p" $ text "Somos una empresa mexicana con más de 43 años dedicados a la comercialización y distribución de refacciones para electrodomésticos, con disponibilidad de stock los 365 días del año y el precio más accesible del mercado."
        el "p" $ text "¡No busques más! Llámanos o visítanos para recibir asesoría técnica especializada y refacciones originales."
      elClass "div" "marcas" $
        forM_ [ "GE", "Easy", "Whirlpool", "Acros", "LG", "Daewoo"
              , "Samsung", "Westinghouse", "Maytag", "Koblenz" ] $ \b ->
          elClass "span" "marca-chip" $ text b

  -- specialties
  espE <- elClass "section" "seccion" $ do
    elClass "p" "eyebrow" $ text "Especialidades"
    elClass "h2" "display seccion-titulo" $ text "Lo que encuentras en el mostrador"
    elClass "div" "especialidades" $ do
      es <- mapM especialidad
        [ ("Refacciones para refrigeradores", Just Refrigeradores)
        , ("Refacciones para lavadoras",      Just Lavadoras)
        , ("Gases refrigerantes",             Just Refrigeradores)
        , ("Gases ecológicos",                Just Refrigeradores)
        , ("Refacciones para licuadoras",     Just Licuadoras)
        , ("Refacciones para secadoras",      Just Secadoras)
        , ("Unidades condensadoras",          Just Refrigeradores)
        , ("Controles",                       Nothing)
        ]
      pure (leftmost es)

  -- featured products
  destE <- elClass "section" "seccion" $ do
    elClass "p" "eyebrow" $ text "Del catálogo"
    elClass "h2" "display seccion-titulo" $ text "Nuestros productos"
    verE <- elClass "div" "grid" $ do
      eventsE <- dyn $ ffor productsDyn $ \ps -> do
        es <- mapM productCardNav (take 8 ps)
        pure (leftmost es)
      switchHold never eventsE
    másE <- elClass "div" "centrado" $ do
      (btn, _) <- elAttr' "button" ("class" =: "cta contorno") $ text "Ver toda la tienda"
      pure (VTienda Nothing <$ domEvent Click btn)
    pure (leftmost [verE, másE])

  -- payment methods: what the online checkout takes vs the counter
  elClass "section" "seccion pagos" $ do
    elClass "p" "eyebrow" $ text "Formas de pago"
    elClass "h2" "display seccion-titulo" $ text "Aceptamos"
    elClass "p" "pagos-grupo" $ text "Paga en línea"
    elClass "div" "pagos-lista" $
      forM_ [ "Visa", "Mastercard", "AMEX", "Tarjeta de Débito"
            , "Apple Pay", "Google Pay", "Oxxo", "SPEI"
            , "Mercado Pago", "Meses sin intereses" ] $ \p ->
        elClass "span" "pago-chip" $ text p
    elClass "p" "pagos-grupo" $ text "En el mostrador"
    elClass "div" "pagos-lista" $
      forM_ [ "Efectivo", "Tarjeta de Crédito", "Tarjeta de Débito", "Carnet"
            , "Transferencia", "Depósito", "Cheque", "Vales", "PayPal", "CoDi"
            , "Seven Eleven", "Samsung Pay", "Claro Pay", "Baz" ] $ \p ->
        elClass "span" "pago-chip" $ text p

  pure (leftmost [heroE, espE, destE])
  where
    especialidad (label, mc) = do
      (e, _) <- elAttr' "button" ("class" =: "especialidad") $ do
        elClass "span" "esp-marca" $ text "●"
        text label
      pure (VTienda mc <$ domEvent Click e)

-- ── info pages (las siete páginas de categoría) ─────────────────────────

infoShell :: MonadWidget t m
          => Dynamic t View -> Dynamic t [Product] -> m (Event t View)
infoShell viewDyn productsDyn = do
  let isInfo = \case VInfo _ -> True; _ -> False
      slugDyn = ffor viewDyn $ \case VInfo s -> Just s; _ -> Nothing
  elDynAttr "div" (shellAttrs isInfo <$> viewDyn) $ do
    navEE <- dyn $ ffor slugDyn $ \case
      Nothing -> pure never
      Just s -> case infoBySlug s of
        Nothing -> pure never
        Just ip -> do
          elClass "section" "seccion" $ do
            elClass "p" "eyebrow" $ text ("Refacciones · " <> ipNav ip)
            elClass "h1" "display seccion-titulo" $ text (ipTitle ip)
            elClass "div" "prosa" $ forM_ (ipParas ip) (el "p" . text)
          railE <- elClass "section" "seccion" $ do
            elClass "p" "eyebrow" $ text "En existencia"
            let relDyn = filter ((== ipCat ip) . category) <$> productsDyn
            verE <- elClass "div" "grid" $ do
              es <- dyn $ ffor relDyn $ \ps ->
                if null ps
                  then do
                    elClass "p" "muted" $
                      text "Consulta existencias de esta línea por teléfono o en el mostrador."
                    pure never
                  else leftmost <$> mapM productCardNav (take 8 ps)
              switchHold never es
            másE <- elClass "div" "centrado" $ do
              (btn, _) <- elAttr' "button" ("class" =: "cta contorno") $
                text ("Ver todo en " <> categoryLabel (ipCat ip))
              pure (VTienda (Just (ipCat ip)) <$ domEvent Click btn)
            pure (leftmost [verE, másE])
          pure railE
    switchHold never navEE

-- ── tienda ──────────────────────────────────────────────────────────────

tiendaShell :: MonadWidget t m
            => Dynamic t View -> Dynamic t [Product]
            -> m (Event t View, Event t [CartOp])
tiendaShell viewDyn productsDyn = do
  let isTienda = \case VTienda _ -> True; _ -> False
      -- category selected via navigation (deep link or especialidad click)
      navCatE = fmapMaybe (\case VTienda mc -> Just mc; _ -> Nothing) (updated viewDyn)
  elDynAttr "div" (shellAttrs isTienda <$> viewDyn) $ do
    elClass "section" "seccion" $ do
      elClass "p" "eyebrow" $ text "Catálogo completo"
      elClass "h1" "display seccion-titulo" $ text "Tienda"

      (searchDyn, catDyn) <- elClass "div" "controls" $ do
        ti <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("class" =: "search" <> "placeholder" =: "Buscar por nombre, marca o SKU…"
               <> "aria-label" =: "Buscar producto")
        catD <- elClass "div" "pills" $ mdo
          allE <- pill selDyn Nothing "Todas"
          catEs <- mapM (\c -> pill selDyn (Just c) (categoryLabel c)) allCategories
          selDyn <- holdDyn Nothing (leftmost (allE : catEs ++ [navCatE]))
          pure selDyn
        pure (_inputElement_value ti, catD)

      let visibleDyn = filterProducts <$> productsDyn <*> searchDyn <*> catDyn

      resE <- elClass "div" "grid" $ do
        resEE <- dyn $ ffor visibleDyn $ \ps ->
          if null ps
            then do
              elClass "p" "muted" $ text "No hay productos que coincidan. Llámanos: seguro lo tenemos en el mostrador."
              pure never
            else leftmost <$> mapM productCardFull ps
        switchHold never resEE

      let navE = fmapMaybe (\case Left v -> Just v; _ -> Nothing) resE
          opsE = fmapMaybe (\case Right op -> Just [op]; _ -> Nothing) resE
      pure (navE, opsE)
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

-- | Card that only navigates to the product page (used on inicio/info).
productCardNav :: MonadWidget t m => Product -> m (Event t View)
productCardNav p = do
  e <- productCardFull p
  pure (fmapMaybe (\case Left v -> Just v; _ -> Nothing) e)

-- | The "ficha de almacén": photo with SKU tag, category stamp, name,
-- brand, mono price, and add-to-cart. Left = navigate, Right = cart op.
productCardFull :: MonadWidget t m => Product -> m (Event t (Either View CartOp))
productCardFull p = elClass "div" "card" $ do
  (fotoE, _) <- elAttr' "div" ("class" =: "foto") $ do
    elClass "span" "sku" $ text (sku p)
    case imageUrl p of
      Just u  -> elAttr "img" ("src" =: u <> "alt" =: name p <> "loading" =: "lazy") blank
      Nothing -> elClass "span" "sinfoto" $ text "Foto en mostrador"
  addE <- elClass "div" "cuerpo" $ do
    elClass "div" "cat" $ text (categoryLabel (category p))
    (nmE, _) <- elClass' "div" "nm" $ text (name p)
    elClass "div" "br" $ text (if T.null (brand p) then "Genérica / multimarca" else brand p)
    elClass "div" "pr" $ text (mxn (priceCents p))
    elClass "div" "st" $ text $
      if stock p > 0
        then "En existencia: " <> T.pack (show (stock p))
        else "Agotado — pregunta en mostrador"
    let attrs = if stock p > 0 then mempty else "disabled" =: "disabled"
    (btn, _) <- elAttr' "button" attrs $ text "Agregar al carrito"
    pure $ leftmost
      [ Right (AddOne (productId p)) <$ domEvent Click btn
      , Left (VProducto (slug p)) <$ domEvent Click nmE
      ]
  pure $ leftmost [addE, Left (VProducto (slug p)) <$ domEvent Click fotoE]

-- ── producto (detalle) ──────────────────────────────────────────────────

productoShell :: MonadWidget t m
              => Dynamic t View -> Dynamic t [Product]
              -> m (Event t View, Event t [CartOp])
productoShell viewDyn productsDyn = do
  let isProd = \case VProducto _ -> True; _ -> False
      prodDyn = ffor ((,) <$> viewDyn <*> productsDyn) $ \(v, ps) -> case v of
        VProducto s -> Data.Maybe.listToMaybe [ p | p <- ps, slug p == s ]
        _           -> Nothing
  elDynAttr "div" (shellAttrs isProd <$> viewDyn) $ do
    resEE <- dyn $ ffor ((,) <$> prodDyn <*> productsDyn) $ \case
      (Nothing, _) -> do
        elClass "section" "seccion" $
          elClass "p" "muted" $ text "Producto no encontrado."
        pure never
      (Just p, ps) -> elClass "section" "seccion" $ do
        migas <- elClass "p" "eyebrow migas" $ do
          (tE, _) <- elClass' "a" "miga" $ text "Tienda"
          text " / "
          (cE, _) <- elClass' "a" "miga" $ text (categoryLabel (category p))
          text (" / " <> sku p)
          pure $ leftmost
            [ Left (VTienda Nothing) <$ domEvent Click tE
            , Left (VTienda (Just (category p))) <$ domEvent Click cE ]
        mainE <- elClass "div" "producto" $ do
          elClass "div" "producto-foto" $
            case imageUrl p of
              Just u  -> elAttr "img" ("src" =: u <> "alt" =: name p) blank
              Nothing -> elClass "span" "sinfoto" $ text "Fotografía disponible en mostrador"
          elClass "div" "producto-info" $ do
            elClass "h1" "display producto-titulo" $ text (name p)
            elClass "p" "muted" $ text
              (if T.null (brand p) then "Genérica / multimarca" else brand p)
            elClass "p" "producto-precio pr-mono" $ text (mxn (priceCents p))
            elClass "p" "st" $ text $
              if stock p > 0
                then "En existencia: " <> T.pack (show (stock p)) <> " piezas"
                else "Agotado — llámanos para apartar la siguiente entrega"
            qDyn <- elClass "div" "cantidad" $ mdo
              (dec, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "−"
              elClass "span" "mono qty-n" $ dynText (T.pack . show <$> qD)
              (inc, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "+"
              qD <- foldDyn ($) 1 $ leftmost
                [ (\q -> max 1 (q - 1)) <$ domEvent Click dec
                , (\q -> min (max 1 (stock p)) (q + 1)) <$ domEvent Click inc ]
              pure qD
            let attrs = if stock p > 0 then "class" =: "cta" else "class" =: "cta" <> "disabled" =: "disabled"
            (btn, _) <- elAttr' "button" attrs $ text "Agregar al carrito"
            pure $ (\q -> Right (AddQty (productId p) q)) <$> current qDyn <@ domEvent Click btn
        relE <- do
          let rel = take 4 [ r | r <- ps, category r == category p, productId r /= productId p ]
          if null rel then pure never else do
            elClass "p" "eyebrow relacionados" $ text "También te puede servir"
            elClass "div" "grid" $ do
              es <- mapM productCardFull rel
              pure (leftmost es)
        pure (leftmost [migas, mainE, relE])
    resE <- switchHold never resEE
    pure ( fmapMaybe (\case Left v -> Just v; _ -> Nothing) resE
         , fmapMaybe (\case Right op -> Just [op]; _ -> Nothing) resE )

-- ── contacto ────────────────────────────────────────────────────────────

contacto :: MonadWidget t m => m (Event t View)
contacto = do
  elClass "section" "seccion" $ do
    elClass "p" "eyebrow" $ text "Contacto"
    elClass "h1" "display seccion-titulo" $
      text "¿Buscas refacciones para electrodomésticos? ¡Contáctanos!"
    elClass "div" "contacto-cols" $ do
      -- form
      elClass "div" "panel" $ do
        nameD <- fieldC "Nombre" "text"
        emailD <- fieldC "Email" "email"
        phoneD <- fieldC "Teléfono" "tel"
        msgD <- elClass "div" "field" $ do
          el "label" $ text "Mensaje"
          ta <- textAreaElement def
          pure (_textAreaElement_value ta)
        rec
          let reqDyn = ContactReq <$> nameD <*> emailD <*> phoneD <*> msgD
              validDyn = (\n e m2 -> not (any (T.null . T.strip) [n, e, m2]))
                <$> nameD <*> emailD <*> msgD
              btnAttrs = ffor ((,) <$> validDyn <*> busyDyn) $ \(ok, busy) ->
                "class" =: "cta" <> if ok && not busy then mempty else "disabled" =: "disabled"
          (sendBtn, _) <- elDynAttr' "button" btnAttrs $ text "Enviar mensaje"
          let submitE = gate (current validDyn) (domEvent Click sendBtn)
          respE <- performRequestAsync (postJsonXhr "/api/contact" <$> current reqDyn <@ submitE)
          busyDyn <- holdDyn False (leftmost [True <$ submitE, False <$ respE])
        let okE  = ffilter ((< 400) . _xhrResponse_status) respE
            badE = ffilter ((>= 400) . _xhrResponse_status) respE
        msgDyn <- holdDyn ("", "") $ leftmost
          [ ("ok", "Gracias por contactarnos. Nos pondremos en contacto contigo tan pronto como sea posible.") <$ okE
          , ("error", "Ups, hubo un error al enviar tu mensaje. Por favor inténtalo de nuevo más tarde.") <$ badE
          , ("", "") <$ submitE
          ]
        elDynAttr "p" ((\(k, _) -> "class" =: k) <$> msgDyn) $
          dynText (snd <$> msgDyn)
      -- info card
      elClass "div" "panel contacto-info" $ do
        elClass "p" "eyebrow" $ text "El mostrador"
        el "p" $ do
          el "strong" $ text "Directo — Refacciones para Electrodomésticos"
          el "br" blank
          text "Zaragoza Pte. 45, Centro"
          el "br" blank
          text "Querétaro, Qro. 76000"
        elAttr "a" ("class" =: "cta contorno mapa"
                 <> "href" =: "https://www.google.com/maps?q=20.586015,-100.397845"
                 <> "target" =: "_blank" <> "rel" =: "noopener") $
          text "Cómo llegar"
        elClass "table" "horario" $ el "tbody" $ do
          el "tr" $ do { el "td" $ text "Lun – Vie"; elClass "td" "mono" $ text "09:00 – 19:00" }
          el "tr" $ do { el "td" $ text "Sábado";    elClass "td" "mono" $ text "09:00 – 14:00" }
          el "tr" $ do { el "td" $ text "Domingo";   elClass "td" "mono" $ text "Cerrado" }
        el "p" $ do
          elAttr "a" ("href" =: "tel:4422154990" <> "class" =: "mono") $ text "442 215 4990"
          el "br" blank
          elAttr "a" ("href" =: "mailto:crelsadecv@aol.com") $ text "crelsadecv@aol.com"
  pure never
  where
    fieldC label typ = elClass "div" "field" $ do
      el "label" $ text label
      ti <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: typ)
      pure (_inputElement_value ti)

-- ── carrito ─────────────────────────────────────────────────────────────

carrito :: MonadWidget t m
        => Dynamic t [Product] -> Dynamic t Cart
        -> m (Event t View, Event t [CartOp])
carrito productsDyn cartDyn = elClass "section" "seccion" $ do
  elClass "p" "eyebrow" $ text "Tu pedido"
  elClass "h1" "display seccion-titulo" $ text "Carrito"
  let linesDyn = cartLines <$> productsDyn <*> cartDyn

  opsE <- el "table" $ do
    el "thead" $ el "tr" $
      forM_ ["Producto", "Precio", "Cantidad", "Subtotal", ""] (el "th" . text)
    opsEE <- el "tbody" $ dyn $ ffor linesDyn $ \ls ->
      if null ls
        then do
          el "tr" $ elAttr "td" ("colspan" =: "5") $
            elClass "span" "muted" $ text "Tu carrito está vacío. Todo el catálogo te espera en la tienda."
          pure never
        else leftmost <$> mapM cartRow ls
    switchHold never opsEE

  let totalDyn = sum . map (\(p, q) -> priceCents p * q) <$> linesDyn
  elClass "div" "total" $ do
    text "Total: "
    el "b" $ dynText (mxn <$> totalDyn)

  (chkBtn, _) <- elAttr' "button" ("class" =: "cta") $ text "Proceder al pago"
  let goCheckout = gate (not . M.null <$> current cartDyn) (domEvent Click chkBtn)
  pure (VCheckout <$ goCheckout, (:[]) <$> opsE)

cartLines :: [Product] -> Cart -> [(Product, Int)]
cartLines ps cart =
  Data.Maybe.mapMaybe (\p -> (p,) <$> M.lookup (productId p) cart) ps

cartRow :: MonadWidget t m => (Product, Int) -> m (Event t CartOp)
cartRow (p, q) = el "tr" $ do
  el "td" $ text (name p)
  elClass "td" "mono" $ text (mxn (priceCents p))
  decE <- el "td" $ do
    (dec, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "−"
    elClass "span" "mono" $ text ("  " <> T.pack (show q) <> "  ")
    (inc, _) <- elAttr' "button" ("class" =: "qtybtn") $ text "+"
    pure $ leftmost
      [ SetQty (productId p) (q - 1) <$ domEvent Click dec
      , SetQty (productId p) (min (stock p) (q + 1)) <$ domEvent Click inc
      ]
  elClass "td" "mono" $ text (mxn (priceCents p * q))
  remE <- el "td" $ do
    (rm, _) <- elAttr' "button" ("class" =: "danger") $ text "Quitar"
    pure (Remove (productId p) <$ domEvent Click rm)
  pure (leftmost [decE, remE])

-- ── checkout ────────────────────────────────────────────────────────────

checkoutShell :: MonadWidget t m
              => Dynamic t View -> Dynamic t [Product] -> Dynamic t Cart
              -> Dynamic t (Maybe UserInfo) -> Dynamic t [Text]
              -> m (Event t View, Event t [CartOp], Event t Text)
checkoutShell viewDyn _productsDyn cartDyn userDyn payProvidersDyn =
  elDynAttr "div" (shellAttrs (== VCheckout) <$> viewDyn) $ elClass "section" "seccion" $ do
    elClass "p" "eyebrow" $ text "Último paso"
    elClass "h1" "display seccion-titulo" $ text "Datos de envío"
    elClass "p" "muted" $
      text "Enviamos a todo México por DHL desde Querétaro."

    -- prefill from the signed-in account (fires on login and at startup)
    let prefillE = fmapMaybe id (updated userDyn)

    (reqDyn, validDyn) <- elClass "div" "panel" $ do
      (nameD, emailD, phoneD) <- elClass "div" "formgrid" $ do
        n <- fieldSet "Nombre completo *" "text" (uiName <$> prefillE)
        e <- fieldSet "Correo electrónico *" "email" (uiEmail <$> prefillE)
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
      -- payment-method chooser (only when more than one provider)
      chosenDyn <- do
        chosenEE <- dyn $ ffor payProvidersDyn $ \ps ->
          if length ps < 2
            then pure never
            else do
              elClass "div" "field" $ el "label" $ text "Método de pago"
              elClass "div" "metodos" $ mdo
                es <- mapM (metodoCard selD) ps
                selD <- holdDyn (headDef "mercadopago" ps) (leftmost es)
                pure (updated selD)
        chosenE <- switchHold never chosenEE
        -- default: first configured provider (or MP) until the user picks
        defaultDyn <- holdDyn Nothing (Just . headDef "mercadopago" <$> updated payProvidersDyn)
        picked <- holdDyn Nothing (Just <$> chosenE)
        pure $ ffor ((,) <$> picked <*> defaultDyn) $ \(p, d) -> maybe d Just p

      let addrDyn = ShippingAddress
            <$> calleD <*> numD <*> colD <*> cpD <*> ciuD <*> estD
            <*> (nonEmpty <$> refD)
          itemsDyn = map (uncurry CartItem) . M.toList <$> cartDyn
          reqD = CheckoutReq
            <$> nameD <*> emailD <*> phoneD <*> addrDyn <*> itemsDyn <*> chosenDyn
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
    headDef d xs = case xs of { (x:_) -> x; [] -> d }
    metodoCard selD p = do
      let (titulo, sub) = case p of
            "stripe"      -> ("Tarjeta, Apple Pay o Google Pay", "Procesado por Stripe")
            "mercadopago" -> ("Mercado Pago", "Tarjetas, OXXO, SPEI y meses sin intereses")
            other         -> (other, "")
          attrs = ffor selD $ \sel ->
            "type" =: "button" <>
            "class" =: (if sel == p then "metodo on" else "metodo")
      (e, _) <- elDynAttr' "button" attrs $ do
        elClass "span" "metodo-titulo" $ text titulo
        elClass "span" "metodo-sub" $ text sub
      pure (p <$ domEvent Click e)
    field label typ = fieldSet label typ never
    fieldSet label typ setE = elClass "div" "field" $ do
      el "label" $ text label
      ti <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: typ)
        & inputElementConfig_setValue .~ setE
      pure (_inputElement_value ti)

-- ── confirmación (sin pago en línea) ────────────────────────────────────

confirmacion :: MonadWidget t m => Dynamic t Text -> m (Event t View)
confirmacion refDyn = elClass "section" "seccion" $ elClass "div" "panel" $ do
  elClass "h2" "h2" $ text "¡Pedido registrado!"
  el "p" $ do
    text "Tu número de pedido es "
    elClass "strong" "mono" $ dynText refDyn
    text "."
  elClass "p" "muted" $
    text "El pago en línea no está disponible en este entorno. Nos pondremos en contacto contigo para coordinar el pago y el envío."
  (btn, _) <- elAttr' "button" ("class" =: "cta") $ text "Volver a la tienda"
  pure (VTienda Nothing <$ domEvent Click btn)

-- ── cuenta (inicio de sesión social y mis pedidos) ──────────────────────

providerLabel :: Text -> Text
providerLabel = \case
  "google"   -> "Google"
  "facebook" -> "Facebook"
  other      -> other

-- | Login screen (provider buttons) or, when signed in, profile + order
-- history. Returns (navigation, logged-out) events.
cuentaView :: MonadWidget t m
           => Dynamic t View -> Dynamic t (Maybe UserInfo) -> Dynamic t [Text]
           -> m (Event t View, Event t ())
cuentaView viewDyn userDyn providersDyn =
  elDynAttr "div" (shellAttrs (== VCuenta) <$> viewDyn) $ elClass "section" "seccion" $ do
    resultEE <- dyn $ ffor ((,) <$> userDyn <*> providersDyn) $ \case
      (Nothing, providers) -> do
        elClass "div" "panel" $ do
          elClass "h2" "h2" $ text "Iniciar sesión"
          if null providers
            then elClass "p" "muted" $
              text "El inicio de sesión no está disponible en este entorno (sin credenciales de proveedor)."
            else do
              elClass "p" "muted" $
                text "Inicia sesión para guardar tus datos y consultar tus pedidos."
              elClass "div" "rowbtns" $ forM_ providers $ \p -> do
                (btn, _) <- elAttr' "button" ("class" =: "cta") $
                  text ("Continuar con " <> providerLabel p)
                performEvent_ $
                  redirectTo ("/api/auth/" <> p <> "/login") <$ domEvent Click btn
        pure never

      (Just user, _) -> do
        elClass "div" "panel" $ do
          elClass "h2" "h2" $ text ("Hola, " <> uiName user)
          elClass "p" "muted" $ text (uiEmail user)
          (outBtn, _) <- elClass "div" "rowbtns" $
            elAttr' "button" mempty $ text "Cerrar sesión"
          outRespE <- performRequestAsync
            (postEmptyXhr "/api/logout" <$ domEvent Click outBtn)

          elClass "h2" "h2" $ text "Mis pedidos"
          pb <- getPostBuild
          ordersE <- getAndDecode (("/api/my-orders" :: Text) <$ pb)
          ordersDyn <- holdDyn [] (fromMaybe [] <$> ordersE)
          el "table" $ do
            el "thead" $ el "tr" $
              forM_ ["Pedido", "Fecha", "Total", "Estado", "Guía DHL"] (el "th" . text)
            el "tbody" $ dyn_ $ ffor ordersDyn $ \os ->
              if null os
                then el "tr" $ elAttr "td" ("colspan" =: "5") $
                       elClass "span" "muted" $ text "Aún no tienes pedidos."
                else forM_ os $ \o -> el "tr" $ do
                  elClass "td" "mono" $ el "strong" $ text (osRef o)
                  el "td" $ text (T.take 10 (T.pack (show (osCreatedAt o))))
                  elClass "td" "mono" $ text (mxn (osTotalCents o))
                  el "td" $ elClass "span" ("badge " <> orderStatusToText (osStatus o)) $
                    text (orderStatusLabel (osStatus o))
                  elClass "td" "mono" $ text (fromMaybe "—" (osWaybill o))
          pure (() <$ outRespE)
    loggedOutE <- switchHold never resultEE
    pure (never, loggedOutE)

-- ── resultado de pago (back-urls de Mercado Pago) ───────────────────────

pagoView :: MonadWidget t m => Dynamic t View -> View -> m (Event t View)
pagoView viewDyn initialView =
  elDynAttr "div" (attrs <$> viewDyn) $ case initialView of
    VPago kind ref -> elClass "section" "seccion" $ do
      elClass "div" "panel" $ do
        elClass "h2" "h2" $ text $ case kind of
          "exito"     -> "¡Gracias por tu compra!"
          "pendiente" -> "Pago en proceso"
          _           -> "El pago no se completó"
        el "p" $ do
          text "Pedido "
          elClass "strong" "mono" $ text ref
        statusDyn <- pollStatus ref
        el "p" $ do
          text "Estado actual: "
          el "strong" $ dynText (maybe "consultando…" orderStatusLabel <$> statusDyn)
        elClass "p" "muted" $ text $ case kind of
          "exito" -> "Recibirás la confirmación del envío por correo."
          _       -> "Si el pago no se concreta, puedes intentarlo de nuevo desde la tienda."
      (btn, _) <- elAttr' "button" ("class" =: "cta") $ text "Volver a la tienda"
      pure (VTienda Nothing <$ domEvent Click btn)
    _ -> pure never
  where
    attrs v = "class" =: "vista" <>
      case v of
        VPago _ _ -> mempty
        _         -> "style" =: "display:none"

pollStatus :: MonadWidget t m => Text -> m (Dynamic t (Maybe OrderStatus))
pollStatus ref = do
  tickE <- tickLossyFromPostBuildTime 3
  respE <- getAndDecode (("/api/orders/" <> ref <> "/status") <$ tickE)
  holdDyn Nothing (fmap osrStatus <$> respE)

-- ── footer ──────────────────────────────────────────────────────────────

footer :: MonadWidget t m => m (Event t View)
footer = elClass "footer" "pie" $ elClass "div" "pie-in" $ do
  elClass "div" "pie-col" $ do
    elClass "p" "display pie-marca" $ text "Directo"
    elClass "p" "pie-txt" $
      text "Refacciones para electrodomésticos originales, al mejor precio de Querétaro desde hace más de 43 años."
  elClass "div" "pie-col" $ do
    elClass "p" "eyebrow claro" $ text "El mostrador"
    elClass "p" "pie-txt" $ do
      text "Zaragoza Pte. 45, Centro"
      el "br" blank
      text "Querétaro, Qro. 76000"
    elAttr "a" ("class" =: "pie-link"
             <> "href" =: "https://www.google.com/maps?q=20.586015,-100.397845"
             <> "target" =: "_blank" <> "rel" =: "noopener") $ text "Cómo llegar"
  elClass "div" "pie-col" $ do
    elClass "p" "eyebrow claro" $ text "Horario"
    elClass "p" "pie-txt mono" $ do
      text "Lun – Vie  09:00 – 19:00"
      el "br" blank
      text "Sábado     09:00 – 14:00"
      el "br" blank
      text "Domingo    Cerrado"
  contE <- elClass "div" "pie-col" $ do
    elClass "p" "eyebrow claro" $ text "Contacto"
    elAttr "a" ("class" =: "pie-link mono" <> "href" =: "tel:4422154990") $
      text "442 215 4990"
    el "br" blank
    elAttr "a" ("class" =: "pie-link" <> "href" =: "mailto:crelsadecv@aol.com") $
      text "crelsadecv@aol.com"
    el "br" blank
    (e, _) <- elClass' "a" "pie-link" $ text "Escríbenos"
    pure (VContacto <$ domEvent Click e)
  pure contE

-- ── storefront-only styles ──────────────────────────────────────────────

clientCss :: Text
clientCss = T.unlines
  [ ".vista{min-height:40vh;}"
  -- header
  , ".cabecera{background:var(--blanco);border-bottom:1px solid var(--linea);position:sticky;top:0;z-index:30;}"
  , ".cabecera-in{max-width:1120px;margin:0 auto;padding:0.7rem 1.4rem;display:flex;align-items:center;gap:1.2rem;flex-wrap:wrap;}"
  , ".logo{display:flex;flex-direction:column;text-decoration:none;color:var(--tinta);}"
  , ".logo-marca{font-size:1.45rem;color:var(--marca);}"
  , ".logo-sub{font-size:0.66rem;color:var(--tinta-suave);letter-spacing:0.04em;}"
  , ".navprincipal{display:flex;align-items:center;gap:0.2rem;flex:1;flex-wrap:wrap;}"
  , ".navlink{background:none;border:none;padding:0.5rem 0.7rem;font-size:0.95rem;color:var(--tinta);cursor:pointer;border-radius:8px;}"
  , ".navlink:hover{background:var(--marca-tinte);color:var(--marca-osc);}"
  , ".menu-refacciones{position:relative;}"
  , ".menu-btn .caret{font-size:0.7rem;margin-left:0.25rem;color:var(--tinta-suave);}"
  , ".menu-lista{display:none;position:absolute;top:100%;left:0;background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);box-shadow:var(--sombra);padding:0.4rem;min-width:230px;z-index:40;}"
  , ".menu-refacciones:hover .menu-lista,.menu-refacciones:focus-within .menu-lista{display:flex;flex-direction:column;align-items:stretch;}"
  , ".menu-lista .navlink{text-align:left;}"
  , ".cabecera-acciones{display:flex;align-items:center;gap:0.6rem;}"
  , ".tel{font-size:0.85rem;color:var(--tinta);text-decoration:none;border:1px solid var(--linea-fuerte);border-radius:8px;padding:0.4rem 0.7rem;}"
  , ".tel:hover{border-color:var(--marca);color:var(--marca-osc);}"
  , ".carrito-btn{background:var(--tinta);color:#fff;border:none;border-radius:8px;padding:0.5rem 0.9rem;cursor:pointer;font-size:0.9rem;font-weight:600;}"
  , ".carrito-btn:hover{background:var(--marca-osc);}"
  -- hero
  , ".hero{background:linear-gradient(100deg,rgba(10,32,30,0.92) 30%,rgba(10,32,30,0.55)),url('/static/site/hero.jpg') center/cover no-repeat;color:#fff;}"
  , ".hero-in{max-width:1120px;margin:0 auto;padding:5rem 1.4rem 4.5rem;animation:subir .5s ease both;}"
  , "@keyframes subir{from{opacity:0;transform:translateY(14px);}to{opacity:1;transform:none;}}"
  , ".hero-eyebrow{color:#7fd6cc;}"
  , ".hero-titulo{font-size:clamp(1.9rem,4.6vw,3.4rem);max-width:19ch;margin-bottom:1rem;}"
  , ".hero-sub{max-width:56ch;opacity:0.88;margin-bottom:1.8rem;}"
  , ".hero-ctas{display:flex;gap:0.8rem;flex-wrap:wrap;}"
  , ".cta.contorno.claro{color:#fff;border-color:rgba(255,255,255,0.7);}"
  , ".cta.contorno.claro:hover{background:rgba(255,255,255,0.12);}"
  -- sections
  , ".seccion{max-width:1120px;margin:0 auto;padding:2.6rem 1.4rem;}"
  , ".seccion-titulo{font-size:clamp(1.4rem,2.8vw,2rem);margin-bottom:1.2rem;max-width:26ch;}"
  , ".dos-cols{display:grid;grid-template-columns:1.4fr 1fr;gap:2rem;align-items:start;}"
  , "@media(max-width:720px){.dos-cols{grid-template-columns:1fr;}}"
  , ".dos-cols p{margin-bottom:0.8rem;max-width:60ch;}"
  , ".marcas{display:flex;flex-wrap:wrap;gap:0.5rem;}"
  , ".marca-chip{border:1px solid var(--linea-fuerte);border-radius:999px;padding:0.3rem 0.9rem;font-size:0.85rem;background:var(--blanco);}"
  , ".especialidades{display:grid;grid-template-columns:repeat(auto-fill,minmax(240px,1fr));gap:0.7rem;}"
  , ".especialidad{display:flex;align-items:center;gap:0.6rem;background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);padding:0.85rem 1rem;font-size:0.95rem;cursor:pointer;text-align:left;color:var(--tinta);transition:border-color .15s ease;}"
  , ".especialidad:hover{border-color:var(--marca);}"
  , ".esp-marca{color:var(--marca);font-size:0.7rem;}"
  , ".centrado{text-align:center;margin-top:1.4rem;}"
  , ".pagos-grupo{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:0.72rem;letter-spacing:0.14em;text-transform:uppercase;color:var(--tinta-suave);margin:1rem 0 0.5rem;}"
  , ".pagos-lista{display:flex;flex-wrap:wrap;gap:0.45rem;}"
  -- payment-method chooser (checkout)
  , ".metodos{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:0.7rem;margin-bottom:1rem;}"
  , ".metodo{display:flex;flex-direction:column;gap:0.2rem;text-align:left;background:var(--blanco);border:2px solid var(--linea);border-radius:var(--radio);padding:0.85rem 1rem;cursor:pointer;}"
  , ".metodo:hover{border-color:var(--marca);}"
  , ".metodo.on{border-color:var(--marca);background:var(--marca-tinte);}"
  , ".metodo-titulo{font-weight:600;color:var(--tinta);}"
  , ".metodo-sub{font-size:0.8rem;color:var(--tinta-suave);}"
  , ".pago-chip{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:0.74rem;letter-spacing:0.04em;border:1px solid var(--linea);background:var(--blanco);border-radius:6px;padding:0.25rem 0.6rem;color:var(--tinta-suave);}"
  , ".prosa p{margin-bottom:0.9rem;max-width:68ch;}"
  -- product page
  , ".migas{color:var(--tinta-suave);}"
  , ".miga{cursor:pointer;color:var(--marca);text-decoration:none;}"
  , ".miga:hover{text-decoration:underline;}"
  , ".producto{display:grid;grid-template-columns:1fr 1fr;gap:2.4rem;align-items:start;margin-bottom:2rem;}"
  , "@media(max-width:720px){.producto{grid-template-columns:1fr;}}"
  , ".producto-foto{background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);display:flex;align-items:center;justify-content:center;aspect-ratio:1;padding:1.5rem;}"
  , ".producto-foto img{max-width:100%;max-height:100%;object-fit:contain;}"
  , ".producto-titulo{font-size:clamp(1.3rem,2.4vw,1.9rem);margin-bottom:0.4rem;}"
  , ".producto-precio{font-size:1.6rem;margin:0.8rem 0 0.3rem;}"
  , ".cantidad{display:flex;align-items:center;gap:0.4rem;margin:1rem 0;}"
  , ".qty-n{min-width:2.2rem;text-align:center;}"
  , ".relacionados{margin-top:1.5rem;}"
  -- contacto
  , ".contacto-cols{display:grid;grid-template-columns:1.3fr 1fr;gap:1.4rem;align-items:start;}"
  , "@media(max-width:720px){.contacto-cols{grid-template-columns:1fr;}}"
  , ".contacto-info .mapa{display:inline-block;text-decoration:none;margin:0.6rem 0 1rem;}"
  , ".horario{margin-bottom:1rem;border:none;}"
  , ".horario td{border:none;padding:0.25rem 0.9rem 0.25rem 0;font-size:0.9rem;}"
  -- footer
  , ".pie{background:var(--tinta);color:#e8efee;margin-top:3rem;}"
  , ".pie-in{max-width:1120px;margin:0 auto;padding:2.6rem 1.4rem;display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:1.6rem;}"
  , ".pie-marca{font-size:1.3rem;color:#7fd6cc;margin-bottom:0.4rem;}"
  , ".pie-txt{font-size:0.88rem;opacity:0.85;line-height:1.7;}"
  , ".eyebrow.claro{color:#7fd6cc;}"
  , ".pie-link{color:#fff;font-size:0.9rem;cursor:pointer;}"
  , ".pie-link:hover{color:#7fd6cc;}"
  ]
