{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex.Dom
import qualified Reflex.Dom.Main as R
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

#ifndef ghcjs_HOST_OS
import qualified Language.Javascript.JSaddle.Warp as JSaddleWrap
#endif

--------------------------------------------------------------------------------
-- Entry

#ifdef ghcjs_HOST_OS
main :: IO ()
main = mainWidget app
#else
main :: IO ()
main = JSaddleWrap.run 3004 (R.mainWidgetWithHead headElement app)
-- main = mainWidgetWithHead headElement app
#endif

headElement :: DomBuilder t m => m ()
headElement = do
  el "title" $ text "Admin • Products & Shipments"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1"
    ) blank
  el "style" $ text css

--------------------------------------------------------------------------------
-- Domain

type PID = Int
type SID = Int

data Product = Product
  { pName  :: Text
  , pPrice :: Double
  , pStock :: Int
  } deriving (Eq, Show)

data ShipmentStatus = Unsent | Sent | Completed
  deriving (Eq, Show)

data Shipment = Shipment
  { shItems  :: Map PID Int      -- product -> quantity
  , shStatus :: ShipmentStatus
  } deriving (Eq, Show)

data Tab = TabProducts | TabShipments
  deriving (Eq, Show)

data Action
  = Add Product
  | Remove PID
  | AdjustStock PID Int          -- ^ delta (can be negative)
  | ToggleShipment SID
  | SetTab Tab                   -- UI event; does not change State

data State = State
  { nextId        :: PID
  , products      :: Map PID Product
  , nextShipment  :: SID
  , shipments     :: Map SID Shipment
  } deriving (Eq, Show)

initState :: State
initState = State
  { nextId = 3
  , products = M.fromList
      [ (1, Product "Laptop" 1299.99 10)
      , (2, Product "Mouse"     29.99  8)
      ]
  , nextShipment = 2
  , shipments = M.fromList
      [ (1, Shipment (M.fromList [(1,1),(2,2)]) Unsent) -- example: 1 laptop, 2 mice
      ]
  }

step :: Action -> State -> State
step (Add p) (State n ps ns ss) =
  State (n+1) (M.insert n p ps) ns ss

step (Remove i) (State n ps ns ss) =
  State n (M.delete i ps) ns ss

step (AdjustStock pid d) (State n ps ns ss) =
  let adj p = let s' = pStock p + d in p { pStock = max 0 s' }
  in State n (M.adjust adj pid ps) ns ss

step (ToggleShipment sid) (State n ps ns ss) =
  case M.lookup sid ss of
    Nothing -> State n ps ns ss
    Just sh ->
      let curr = shStatus sh
          next = case curr of
                   Unsent    -> Sent
                   Sent      -> Completed
                   Completed -> Completed
          -- Reduce inventory exactly once on Unsent -> Sent
          ps'  = case (curr, next) of
                   (Unsent, Sent) ->
                     M.foldlWithKey'
                       (\acc pid q -> M.adjust (\p -> p { pStock = max 0 (pStock p - q) }) pid acc)
                       ps
                       (shItems sh)
                   _ -> ps
          ss' = M.insert sid (sh { shStatus = next }) ss
      in State n ps' ns ss'

step (SetTab _) st = st  -- UI-only

--------------------------------------------------------------------------------
-- App

app :: MonadWidget t m => m ()
app = el "div" $ mdo
  -- Header (sticky) – matches client style
  headerTabEv <- headerBar

  -- Current tab (default: Products)
  tabD <- foldDyn (\(SetTab t) _ -> t) TabProducts headerTabEv

  -- KPI strip
  kpiStrip (products <$> st) (shipments <$> st)

  -- Sections (both rendered; CSS toggles visibility)
  let isProdD = (== TabProducts)  <$> tabD
      isShipD = (== TabShipments) <$> tabD

  prodEv <- section isProdD $ do
    elClass "h2" "page-title" $ text "Products"
    productsUI (products <$> st)

  shipEv <- section isShipD $ do
    elClass "h2" "page-title" $ text "Shipments"
    shipmentsUI st

  -- Global state fold
  st <- foldDyn step initState (leftmost [prodEv, shipEv])

  pure ()

--------------------------------------------------------------------------------
-- Header / Tabs (client-style)

headerBar :: MonadWidget t m => m (Event t Action)
headerBar = elClass "div" "header" $ do
  elClass "div" "header-content" $ do
    elClass "div" "logo" $ text "🛠️ Admin"
    elClass "nav" "nav" $ do
      prodE <- tabLink "Products"
      shipE <- tabLink "Shipments"
      pure $ leftmost [SetTab TabProducts <$ prodE, SetTab TabShipments <$ shipE]
  where
    tabLink :: MonadWidget t m => Text -> m (Event t ())
    tabLink label = do
      (e, _) <- elAttr' "span" ("class" =: "nav-item") $ text label
      pure (domEvent Click e)

--------------------------------------------------------------------------------
-- KPI strip

kpiStrip :: MonadWidget t m
         => Dynamic t (Map PID Product)
         -> Dynamic t (Map SID Shipment)
         -> m ()
kpiStrip psD shD = elClass "div" "kpi-container" $ do
  let totalSkusD  = M.size <$> psD
      totalStockD = ffor psD $ \ps -> sum (pStock <$> M.elems ps)
      unsentD     = ffor shD $ \ss -> length [ () | s <- M.elems ss, shStatus s == Unsent ]
      sentD       = ffor shD $ \ss -> length [ () | s <- M.elems ss, shStatus s == Sent ]
      doneD       = ffor shD $ \ss -> length [ () | s <- M.elems ss, shStatus s == Completed ]
  kpi "Total SKUs" (tshow <$> totalSkusD)
  kpi "Total Stock" (tshow <$> totalStockD)
  kpi "Unsent" (tshow <$> unsentD)
  kpi "In transit" (tshow <$> sentD)
  kpi "Completed" (tshow <$> doneD)
  where
    kpi :: MonadWidget t m => Text -> Dynamic t Text -> m ()
    kpi label valD = elClass "div" "kpi-card" $ do
      elClass "div" "kpi-label" $ text label
      elClass "div" "kpi-value" $ dynText valD

--------------------------------------------------------------------------------
-- Products UI

productsUI :: MonadWidget t m
           => Dynamic t (Map PID Product)
           -> m (Event t Action)
productsUI psD = elClass "div" "container" $ do
  addEv   <- addForm
  tableEv <- productTable psD
  pure (leftmost [addEv, tableEv])

addForm :: MonadWidget t m => m (Event t Action)
addForm = elClass "div" "card" $ do
  elClass "h3" "card-title" $ text "Add product"

  nameD  <- row "Name"  $ _textInput_value <$> textInput def
  priceD <- row "Price" $ _textInput_value <$> textInput def { _textInputConfig_inputType = "number" }
  stockD <- row "Stock" $ _textInput_value <$> textInput def { _textInputConfig_inputType = "number" }

  let priceParsed = fromMaybe 0 . readMaybe . T.unpack <$> priceD
      stockParsed = fromMaybe 0 . readMaybe . T.unpack <$> stockD
      prodD       = Product <$> nameD <*> priceParsed <*> stockParsed

  addBtn <- elClass "div" "row" $ primaryButton "Add"
  pure $ Add <$> tagPromptlyDyn prodD addBtn

productTable :: MonadWidget t m => Dynamic t (Map PID Product) -> m (Event t Action)
productTable psD = elClass "div" "card" $ do
  elClass "h3" "card-title" $ text "Inventory"
  elClass "div" "table-wrap" $ do
    el "table" $ do
      el "thead" $ el "tr" $ do
        th "ID" >> th "Name" >> th "Price" >> th "Stock" >> th "Actions"
      el "tbody" $ do
        rows <- listWithKey psD $ \pid pDyn -> el "tr" $ do
          el "td" $ text (tshow pid)
          el "td" $ dynText (pName <$> pDyn)
          el "td" $ dynText (("$" <>) . tshow . pPrice <$> pDyn)
          el "td" $ dynText (tshow . pStock <$> pDyn)
          ev <- el "td" $ do
            rm <- dangerButton "Remove"
            pure (Remove pid <$ rm)
          pure ev
        pure . switchDyn $ fmap (leftmost . M.elems) rows
  where
    th :: MonadWidget t m => Text -> m ()
    th = el "th" . text

--------------------------------------------------------------------------------
-- Shipments UI

shipmentsUI :: MonadWidget t m => Dynamic t State -> m (Event t Action)
shipmentsUI stD = elClass "div" "container" $ do
  elClass "div" "card" $ do
    elClass "h3" "card-title" $ text "Shipments"
    let shipsD = shipments <$> stD
        prodsD = products  <$> stD
    elClass "div" "table-wrap" $ do
      el "table" $ do
        el "thead" $ el "tr" $ do
          th "SID" >> th "Status" >> th "Items" >> th "Action"
        el "tbody" $ do
          rows <- listWithKey shipsD $ \sid shDyn -> el "tr" $ do
            -- ID
            el "td" $ text (tshow sid)
            -- Status badge
            el "td" $ dyn_ $ ffor (shStatus <$> shDyn) $ \s ->
              elAttr "span" ("class" =: ("badge " <> statusClass s)) $
                text (statusLabel s)
            -- Items pretty print
            el "td" $ dyn_ $ ffor2 shDyn prodsD $ \sh ps -> do
              let toPiece (pid, q) =
                    let nameTxt = maybe ("#" <> tshow pid) pName (M.lookup pid ps)
                    in tshow q <> "× " <> nameTxt
                  pieces = map toPiece (M.toList (shItems sh))
              text (T.intercalate ", " pieces)
            -- Action button
            el "td" $ do
              let isCompletedD = (== Completed) . shStatus <$> shDyn
                  btnLabelD    = ffor (shStatus <$> shDyn) $ \s ->
                                   case s of
                                     Unsent    -> "Mark as Sent"
                                     Sent      -> "Mark as Completed"
                                     Completed -> "Completed"
              -- disabled style + ignore clicks when completed
              (btnEl, _) <- elDynAttr' "button" (ffor isCompletedD $ \dis ->
                                  if dis then "class" =: "btn btn-disabled"
                                         else "class" =: "btn btn-primary") $
                                dynText btnLabelD
              let clickEv = domEvent Click btnEl
                  guarded  = gate (current (not <$> isCompletedD)) clickEv
              pure (ToggleShipment sid <$ guarded)

          pure . switchDyn $ fmap (leftmost . M.elems) rows
  where
    th :: MonadWidget t m => Text -> m ()
    th = el "th" . text

    statusLabel :: ShipmentStatus -> Text
    statusLabel Unsent    = "Unsent"
    statusLabel Sent      = "Sent"
    statusLabel Completed = "Completed"

    statusClass :: ShipmentStatus -> Text
    statusClass Unsent    = "badge-gray"
    statusClass Sent      = "badge-blue"
    statusClass Completed = "badge-green"

--------------------------------------------------------------------------------
-- Small UI helpers

primaryButton :: MonadWidget t m => Text -> m (Event t ())
primaryButton label = do
  (e, _) <- elAttr' "button" ("class" =: "btn btn-primary") $ text label
  pure (domEvent Click e)

dangerButton :: MonadWidget t m => Text -> m (Event t ())
dangerButton label = do
  (e, _) <- elAttr' "button" ("class" =: "btn btn-danger") $ text label
  pure (domEvent Click e)

row :: MonadWidget t m => Text -> m a -> m a
row lbl inner = elClass "div" "row" $ el "label" (text lbl) >> inner

section :: MonadWidget t m => Dynamic t Bool -> m a -> m a
section visD inner =
  let toStyle True  = "display:block"
      toStyle False = "display:none"
      attrs = ffor visD $ \v -> "class" =: "container" <> "style" =: toStyle v
  in elDynAttr "div" attrs inner

tshow :: Show a => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- CSS (aligned with client app)

css :: Text
css = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f8f9fa; color:#2c3e50; }"
  , ".header { position: sticky; top:0; z-index:999; background: #1a1a2e; color: white; padding: 0.75rem 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }"
  , ".header-content { max-width: 1200px; margin: 0 auto; padding: 0 1rem; display: flex; justify-content: space-between; align-items: center; }"
  , ".logo { font-size: 1.25rem; font-weight: 700; letter-spacing: 0.2px; }"
  , ".nav { display: flex; gap: 1.25rem; align-items: center; }"
  , ".nav-item { color: white; cursor: pointer; transition: opacity 0.25s; user-select: none; }"
  , ".nav-item:hover { opacity: 0.85; }"
  , ".container { max-width: 1200px; margin: 1.25rem auto; padding: 0 1rem; }"
  , ".page-title { margin: 0.25rem 0 0.75rem; font-size: 1.25rem; color: #2c3e50; }"
  , ".kpi-container { max-width: 1200px; margin: 1rem auto 0; padding: 0 1rem; display: grid; grid-template-columns: repeat(auto-fit, minmax(160px,1fr)); gap: 0.75rem; }"
  , ".kpi-card { background: white; border: 1px solid #ecf0f1; border-radius: 10px; padding: 0.75rem 0.9rem; box-shadow: 0 2px 8px rgba(0,0,0,0.06); }"
  , ".kpi-label { font-size: 0.8rem; color:#7f8c8d; margin-bottom: 0.15rem; }"
  , ".kpi-value { font-weight: 700; font-size: 1.15rem; }"
  , ".card { background: white; border: 1px solid #ecf0f1; border-radius: 12px; padding: 1rem; margin: 1rem 0; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }"
  , ".card-title { font-size: 1rem; margin-bottom: 0.75rem; }"
  , ".row { display: grid; grid-template-columns: 140px 1fr; gap: 12px; align-items: center; margin: 10px 0; }"
  , "label { font-size: 0.9rem; color:#7f8c8d; }"
  , "input[type=text], input[type=number] { width: 100%; padding: 0.65rem 0.75rem; font-size: 0.95rem; border: 1px solid #ddd; border-radius: 8px; outline: none; }"
  , "input[type=text]:focus, input[type=number]:focus { border-color:#3498db; box-shadow: 0 0 0 3px rgba(52,152,219,0.15); }"
  , ".table-wrap { overflow: auto; border-radius: 8px; }"
  , "table { width: 100%; border-collapse: collapse; background: #fff; }"
  , "thead th { background:#2d6cdf; color:#fff; text-align:left; font-weight:600; letter-spacing:0.2px; padding: 10px; font-size: 0.9rem; }"
  , "tbody td { border-bottom:1px solid #ecf0f1; padding: 10px; font-size: 0.95rem; }"
  , "tbody tr:hover { background: #fbfcff; }"
  , ".btn { padding: 0.5rem 0.8rem; border-radius: 8px; border: 1px solid transparent; cursor: pointer; font-size: 0.95rem; }"
  , ".btn-primary { background:#3498db; color:#fff; }"
  , ".btn-primary:hover { background:#2980b9; }"
  , ".btn-danger { background:#e74c3c; color:#fff; }"
  , ".btn-danger:hover { background:#cf3e30; }"
  , ".btn-disabled { background:#bdc3c7; color:#fff; cursor: not-allowed; }"
  , ".badge { display:inline-block; padding: 0.2rem 0.55rem; border-radius: 999px; font-size: 0.8rem; font-weight: 600; }"
  , ".badge-gray { background:#ecf0f1; color:#2c3e50; }"
  , ".badge-blue { background:#d6e4ff; color:#2d6cdf; }"
  , ".badge-green { background:#d5f5e3; color:#27ae60; }"
  ]
