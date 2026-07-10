-- | directo — admin panel SPA (productos, pedidos, guía DHL).
module Main where

import           Control.Monad        (forM_, void)
import           Data.Int             (Int64)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Reflex.Dom

import           Common
import           Directo.Types

main :: IO ()
main = mainWidgetWithHead headW bodyW

headW :: DomBuilder t m => m ()
headW = do
  el "title" $ text "Directo — Administración"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport"
              <> "content" =: "width=device-width, initial-scale=1") blank
  el "style" $ text baseCss

bodyW :: MonadWidget t m => m ()
bodyW = do
  pb <- getPostBuild
  meE <- performRequestAsync (getJson "/api/admin/me" <$ pb)
  let authedE = ffor meE $ \r -> _xhrResponse_status r == 200
  void $ widgetHold (elClass "p" "muted" $ text "Cargando…") $
    ffor authedE $ \ok -> if ok then panel else loginScreen

loginScreen :: MonadWidget t m => m ()
loginScreen = elClass "div" "wrap" $ elClass "div" "panel" $ do
  elClass "h2" "h2" $ text "Administración — Directo"
  rec
    pwDyn <- elClass "div" "field" $ do
      el "label" $ text "Contraseña"
      ti <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: "password")
      pure (_inputElement_value ti)
    (btn, _) <- elAttr' "button" ("class" =: "cta") $ text "Entrar"
    let submitE = domEvent Click btn
    respE <- performRequestAsync
      ((postJsonXhr "/api/admin/login" . LoginRequest) <$> current pwDyn <@ submitE)
    let okE   = ffor respE $ \r -> _xhrResponse_status r == 200
    errDyn <- holdDyn "" $ ffor okE $ \ok ->
      if ok then "" else "Contraseña incorrecta."
    elClass "p" "error" $ dynText errDyn
  void $ widgetHold blank $ ffor (ffilter id ((\r -> _xhrResponse_status r == 200) <$> respE)) $
    \_ -> panel
  pure ()

-- ── panel ───────────────────────────────────────────────────────────────

data Tab = TabProductos | TabPedidos deriving (Eq, Show)

panel :: MonadWidget t m => m ()
panel = do
  rec
    tabE <- elClass "header" "topbar" $ do
      elClass "div" "brand" $ do
        text "DIRECTO · Admin"
        el "small" $ text "Productos, pedidos y guías DHL"
      elClass "div" "spacer" blank
      (prodBtn, _) <- elAttr' "button" ("class" =: "navbtn") $ text "Productos"
      (ordBtn, _)  <- elAttr' "button" ("class" =: "navbtn") $ text "Pedidos"
      (outBtn, _)  <- elAttr' "button" ("class" =: "navbtn") $ text "Salir"
      logoutE <- performRequestAsync (postEmptyXhr "/api/admin/logout" <$ domEvent Click outBtn)
      void $ widgetHold blank (loginScreen <$ logoutE)
      pure $ leftmost
        [ TabProductos <$ domEvent Click prodBtn
        , TabPedidos   <$ domEvent Click ordBtn
        ]
    tabDyn <- holdDyn TabProductos tabE
  elDynAttr "div" (tabAttrs TabProductos <$> tabDyn) productosTab
  elDynAttr "div" (tabAttrs TabPedidos <$> tabDyn) pedidosTab
  where
    tabAttrs me t =
      "class" =: "wrap" <> if t == me then mempty else "style" =: "display:none"

-- ── productos ───────────────────────────────────────────────────────────

productosTab :: MonadWidget t m => m ()
productosTab = do
  pb <- getPostBuild
  rec
    let fetchE = leftmost [() <$ pb, () <$ mutatedE]
    productsE <- getAndDecode (("/api/admin/products" :: Text) <$ fetchE)
    productsDyn <- holdDyn [] (fromMaybe [] <$> productsE)

    -- KPIs
    elClass "div" "kpis" $ do
      kpi "SKUs activos" (T.pack . show . length . filter active <$> productsDyn)
      kpi "Piezas en stock" (T.pack . show . sum . map stock . filter active <$> productsDyn)

    -- edit/create form (rebuilt when the editing target changes)
    editDyn <- holdDyn Nothing (leftmost [Just <$> editReqE, Nothing <$ mutatedE])
    formEE <- elClass "div" "panel" $ dyn (productForm <$> editDyn)
    mutatedE' <- switchHold never formEE

    -- table
    editReqE <- el "table" $ do
      el "thead" $ el "tr" $
        forM_ ["SKU", "Producto", "Categoría", "Marca", "Precio", "Stock", "Activo", ""]
          (el "th" . text)
      rowsEE <- el "tbody" $ dyn $ ffor productsDyn $ \ps ->
        leftmost <$> mapM productRow ps
      switchHold never rowsEE

    let mutatedE = mutatedE'
  pure ()
  where
    kpi label valDyn = elClass "div" "kpi" $ do
      elClass "div" "n" $ dynText valDyn
      elClass "div" "l" $ text label

productRow :: MonadWidget t m => Product -> m (Event t Product)
productRow p = el "tr" $ do
  el "td" $ text (sku p)
  el "td" $ text (name p)
  el "td" $ text (categoryLabel (category p))
  el "td" $ text (brand p)
  el "td" $ text (mxn (priceCents p))
  el "td" $ text (T.pack (show (stock p)))
  el "td" $ text (if active p then "Sí" else "No")
  el "td" $ do
    (btn, _) <- elAttr' "button" ("class" =: "navbtn" <> "style" =: "color:#12365f;border-color:#cdd5df") $
      text "Editar"
    pure (p <$ domEvent Click btn)

-- | Create (Nothing) or edit (Just p). Returns an event that fires after a
-- successful save/delete so the caller refetches.
productForm :: MonadWidget t m => Maybe Product -> m (Event t ())
productForm mp = do
  elClass "h2" "h2" $ text (maybe "Nuevo producto" (("Editar: " <>) . name) mp)
  let ini f dflt = maybe dflt f mp
  rec
    (skuD, nameD, brandD) <- elClass "div" "formgrid" $ do
      s <- field "SKU" (ini sku "")
      n <- field "Nombre" (ini name "")
      b <- field "Marca" (ini brand "")
      pure (s, n, b)
    (catD, priceD, stockD, weightD) <- elClass "div" "formgrid" $ do
      c <- categoryField (ini category Otros)
      pr <- field "Precio (MXN, ej. 495.00)" (ini (centsToPrice . priceCents) "")
      st <- field "Stock" (ini (T.pack . show . stock) "0")
      w  <- field "Peso (gramos)" (ini (maybe "" (T.pack . show) . weightGrams) "")
      pure (c, pr, st, w)
    activeD <- elClass "div" "field" $ do
      el "label" $ text "Activo"
      cb <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("type" =: "checkbox")
        & inputElementConfig_initialChecked .~ ini active True
      pure (_inputElement_checked cb)

    let inputDyn = mkInput
          <$> skuD <*> nameD <*> brandD <*> catD
          <*> priceD <*> stockD <*> weightD <*> activeD

    (saveE, deleteE) <- elClass "div" "rowbtns" $ do
      (sv, _) <- elAttr' "button" ("class" =: "cta") $
        text (maybe "Crear producto" (const "Guardar cambios") mp)
      delE <- case mp of
        Nothing -> pure never
        Just p  -> do
          (dl, _) <- elAttr' "button" mempty $ text "Desactivar"
          pure (productId p <$ domEvent Click dl)
      pure (domEvent Click sv, delE)

    saveRespE <- performRequestAsync $
      (\inp -> case mp of
          Nothing -> postJsonXhr "/api/admin/products" inp
          Just p  -> putJsonXhr ("/api/admin/products/" <> tshow (productId p)) inp)
      <$> current inputDyn <@ saveE
    delRespE <- performRequestAsync $
      (\pid -> deleteXhr ("/api/admin/products/" <> tshow pid)) <$> deleteE

    let doneE = leftmost
          [ () <$ ffilter (\r -> _xhrResponse_status r < 300) saveRespE
          , () <$ delRespE
          ]
    errDyn <- holdDyn "" $ leftmost
      [ ffor saveRespE $ \r ->
          if _xhrResponse_status r < 300 then "" else "No se pudo guardar (verifica los campos)."
      , "" <$ saveE
      ]
    elClass "p" "error" $ dynText errDyn
  pure doneE
  where
    tshow = T.pack . show
    centsToPrice c =
      let (ps, ct) = c `divMod` 100
      in T.pack (show ps) <> "." <> T.justifyRight 2 '0' (T.pack (show ct))
    priceToCents t =
      let s = T.strip (T.replace "," "" (T.replace "$" "" t))
          (whole, frac0) = T.breakOn "." s
          frac = T.take 2 (T.drop 1 frac0 <> "00")
          readInt x = if T.all (`elem` ("0123456789" :: String)) x && not (T.null x)
                        then Just (read (T.unpack x) :: Int) else Nothing
      in case (readInt whole, readInt frac) of
           (Just w, Just f) -> w * 100 + f
           _                -> 0
    readIntD t = fromMaybe 0 $
      if T.all (`elem` ("0123456789" :: String)) s && not (T.null s)
        then Just (read (T.unpack s) :: Int) else Nothing
      where s = T.strip t
    mkInput s n b c pr st w act = ProductInput
      { piSku = T.strip s
      , piName = T.strip n
      , piDescription = ""
      , piCategory = c
      , piBrand = T.strip b
      , piPriceCents = priceToCents pr
      , piStock = readIntD st
      , piWeightGrams = let g = readIntD w in if T.null (T.strip w) then Nothing else Just g
      , piImageUrl = Nothing
      , piActive = act
      }
    field label initial = elClass "div" "field" $ do
      el "label" $ text label
      ti <- inputElement $ def
        & inputElementConfig_initialValue .~ initial
      pure (_inputElement_value ti)
    categoryField initial = elClass "div" "field" $ do
      el "label" $ text "Categoría"
      dd <- selectElement
        (def & selectElementConfig_initialValue .~ T.pack (show initial))
        (forM_ allCategories $ \c ->
          elAttr "option" ("value" =: T.pack (show c)) $ text (categoryLabel c))
      pure (fromMaybe Otros . categoryFromText <$> _selectElement_value (fst dd))

-- ── pedidos ─────────────────────────────────────────────────────────────

pedidosTab :: MonadWidget t m => m ()
pedidosTab = do
  pb <- getPostBuild
  rec
    filterDyn <- elClass "div" "controls" $ do
      el "span" $ text "Filtrar por estado:"
      dd <- selectElement (def & selectElementConfig_initialValue .~ "todos") $ do
        elAttr "option" ("value" =: "todos") $ text "Todos"
        forM_ [minBound .. maxBound :: OrderStatus] $ \s ->
          elAttr "option" ("value" =: orderStatusToText s) $ text (orderStatusLabel s)
      pure (_selectElement_value (fst dd))

    let urlDyn = ffor filterDyn $ \f ->
          if f == "todos" then "/api/admin/orders"
                          else "/api/admin/orders?status=" <> f
        fetchE = leftmost
          [ tag (current urlDyn) pb
          , updated urlDyn
          , tag (current urlDyn) (() <$ actionDoneE)
          ]
    ordersE <- getAndDecode fetchE
    ordersDyn <- holdDyn [] (fromMaybe [] <$> ordersE)

    selectE <- el "table" $ do
      el "thead" $ el "tr" $
        forM_ ["Pedido", "Fecha", "Cliente", "Total", "Estado", "Guía DHL", ""]
          (el "th" . text)
      rowsEE <- el "tbody" $ dyn $ ffor ordersDyn $ \os ->
        if null os
          then do
            el "tr" $ elAttr "td" ("colspan" =: "7") $
              elClass "span" "muted" $ text "Sin pedidos."
            pure never
          else leftmost <$> mapM orderRow os
      switchHold never rowsEE

    selDyn <- holdDyn Nothing (Just <$> selectE)
    let detailFetchE = leftmost
          [ fmapMaybe id (updated selDyn)
          , attachWithMaybe (\sel _ -> sel) (current selDyn) actionDoneE
          ]
    detailE <- getAndDecode
      ((\oid -> "/api/admin/orders/" <> T.pack (show oid)) <$> detailFetchE)
    detailDyn <- holdDyn Nothing detailE

    actionEE <- dyn (orderDetail <$> detailDyn)
    actionDoneE <- switchHold never actionEE
  pure ()

orderRow :: MonadWidget t m => OrderSummary -> m (Event t Int64)
orderRow o = el "tr" $ do
  el "td" $ el "strong" $ text (osRef o)
  el "td" $ text (T.take 10 (T.pack (show (osCreatedAt o))))
  el "td" $ text (osCustomer o)
  el "td" $ text (mxn (osTotalCents o))
  el "td" $ elClass "span" ("badge " <> orderStatusToText (osStatus o)) $
    text (orderStatusLabel (osStatus o))
  el "td" $ text (fromMaybe "—" (osWaybill o))
  el "td" $ do
    (btn, _) <- elAttr' "button" ("class" =: "navbtn" <> "style" =: "color:#12365f;border-color:#cdd5df") $
      text "Ver"
    pure (osId o <$ domEvent Click btn)

-- | Detail panel with state-machine actions and the DHL sheet. Returns an
-- event firing after any successful mutation.
orderDetail :: MonadWidget t m => Maybe OrderDetail -> m (Event t ())
orderDetail Nothing = do
  elClass "p" "muted" $ text "Selecciona un pedido para ver el detalle."
  pure never
orderDetail (Just d) = elClass "div" "panel" $ do
  let s   = odSummary d
      oid = T.pack (show (osId s))
      st  = osStatus s
  elClass "h2" "h2" $ do
    text ("Pedido " <> osRef s <> " — ")
    elClass "span" ("badge " <> orderStatusToText st) $ text (orderStatusLabel st)

  elClass "div" "formgrid" $ do
    elClass "div" "field" $ do
      el "label" $ text "Cliente"
      el "div" $ text (osCustomer s)
      elClass "div" "muted" $ text (odEmail d <> " · " <> odPhone d)
    elClass "div" "field" $ do
      el "label" $ text "Dirección de entrega"
      el "div" $ text (addressOneLine (odAddress d))
    elClass "div" "field" $ do
      el "label" $ text "Pago"
      el "div" $ text $
        fromMaybe "—" (odMpStatus d) <> maybe "" (" · " <>) (odMpPayment d)

  el "table" $ do
    el "thead" $ el "tr" $
      forM_ ["SKU", "Producto", "Cantidad", "Precio unitario", "Importe"] (el "th" . text)
    el "tbody" $ forM_ (odLines d) $ \l -> el "tr" $ do
      el "td" $ text (olProductSku l)
      el "td" $ text (olProductName l)
      el "td" $ text (T.pack (show (olQuantity l)))
      el "td" $ text (mxn (olUnitPriceCents l))
      el "td" $ text (mxn (olUnitPriceCents l * olQuantity l))
  elClass "div" "total" $ text ("Total: " <> mxn (osTotalCents s))

  rec
    waybillDyn <- if st == Paid
      then elClass "div" "field" $ do
        el "label" $ text "Número de guía DHL (al enviar)"
        ti <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
              ("placeholder" =: "p. ej. 1234567890")
        pure (_inputElement_value ti)
      else pure (constDyn "")

    actionE <- elClass "div" "rowbtns" $ do
      markPaidE <- actionButton (st `elem` [Created, PendingPayment])
        "Marcar pagado" ("/api/admin/orders/" <> oid <> "/mark-paid")
      shipE <- if st == Paid
        then do
          (btn, _) <- elAttr' "button" mempty $ text "Marcar enviado"
          let clickE = gate (not . T.null . T.strip <$> current waybillDyn) (domEvent Click btn)
          respE <- performRequestAsync $
            (\w -> postJsonXhr ("/api/admin/orders/" <> oid <> "/ship") (ShipReq (T.strip w)))
            <$> current waybillDyn <@ clickE
          pure (() <$ ffilter (\r -> _xhrResponse_status r < 300) respE)
        else pure never
      completeE <- actionButton (st == Shipped)
        "Completar" ("/api/admin/orders/" <> oid <> "/complete")
      cancelE <- actionButton (st `elem` [Created, PendingPayment, Paid])
        "Cancelar pedido" ("/api/admin/orders/" <> oid <> "/cancel")
      pure (leftmost [markPaidE, shipE, completeE, cancelE])

  guiaSection st oid

  pure actionE
  where
    actionButton enabled label url =
      if not enabled then pure never else do
        (btn, _) <- elAttr' "button" mempty $ text label
        respE <- performRequestAsync (postEmptyXhr url <$ domEvent Click btn)
        pure (() <$ ffilter (\r -> _xhrResponse_status r < 300) respE)

addressOneLine :: ShippingAddress -> Text
addressOneLine a = T.intercalate ", " $
  [ calle a <> " " <> numero a
  , "Col. " <> colonia a
  , "CP " <> codigoPostal a
  , ciudad a
  , estado a
  ] <> maybe [] (\r -> ["Ref: " <> r]) (referencias a)

-- | Fetch and render the printable/copyable DHL sheet.
guiaSection :: MonadWidget t m => OrderStatus -> Text -> m ()
guiaSection st oid
  | st `notElem` [Paid, Shipped, Completed] = pure ()
  | otherwise = do
      rec
        (btn, _) <- elClass "div" "rowbtns" $
          elAttr' "button" mempty $ text "Generar guía DHL"
        guiaE <- getAndDecode (("/api/admin/orders/" <> oid <> "/guia") <$ domEvent Click btn)
        guiaDyn <- holdDyn Nothing guiaE
      void $ dyn $ ffor guiaDyn $ \case
        Nothing -> blank
        Just g  -> do
          let txt = guiaText g
          elClass "div" "guia" $ text txt
          elClass "div" "rowbtns noprint" $ do
            (pr, _) <- elAttr' "button" mempty $ text "Imprimir"
            performEvent_ (printPage <$ domEvent Click pr)
            (cp, _) <- elAttr' "button" mempty $ text "Copiar"
            performEvent_ (copyToClipboard txt <$ domEvent Click cp)

guiaText :: GuiaDhl -> Text
guiaText g = T.unlines $
  [ "═══ GUÍA DE ENVÍO DHL — DIRECTO ═══"
  , ""
  , "Referencia:      " <> gReference g
  , ""
  , "── REMITENTE ──"
  , saName (gSender g)
  ] <> saAddressLines (gSender g) <>
  [ "Tel: " <> saPhone (gSender g)
  , ""
  , "── DESTINATARIO ──"
  , gRecipientName g
  , addressOneLine (gRecipientAddress g)
  , "Tel: " <> gRecipientPhone g
  , ""
  , "── CONTENIDO ──"
  ] <>
  [ "  " <> T.pack (show (olQuantity l)) <> " × " <> olProductName l
      <> " (" <> olProductSku l <> ")"
  | l <- gItems g
  ] <>
  [ ""
  , "Peso estimado:   " <> kg (gTotalWeightGrams g)
  , "Valor declarado: " <> mxn (gDeclaredValueCents g)
  ]
  where
    kg grams = T.pack (show (fromIntegral grams / 1000 :: Double)) <> " kg"
