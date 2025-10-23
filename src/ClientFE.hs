{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}

module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM_, forM, when)
import Text.Printf (printf)

-- Main entry point
main :: IO ()
main =
#ifdef GHCJS_BROWSER
  mainWidget app
#else
  mainWidgetWithHead headElement app
#endif

-- HTML head element
headElement :: DomBuilder t m => m ()
headElement = do
  el "title" $ text "Reflex Store"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1"
    ) blank
  el "style" $ text css

-- CSS styles
css :: Text
css = T.unlines
  [ "* { box-sizing: border-box; margin: 0; padding: 0; }"
  , "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f8f9fa; }"
  , ".header { background: #1a1a2e; color: white; padding: 1rem 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }"
  , ".header-content { max-width: 1200px; margin: 0 auto; padding: 0 1rem; display: flex; justify-content: space-between; align-items: center; }"
  , ".logo { font-size: 1.5rem; font-weight: bold; }"
  , ".nav { display: flex; gap: 2rem; align-items: center; }"
  , ".nav-item { color: white; cursor: pointer; transition: opacity 0.3s; }"
  , ".nav-item:hover { opacity: 0.8; }"
  , ".cart-badge { background: #e74c3c; color: white; border-radius: 50%; padding: 0.2rem 0.5rem; font-size: 0.875rem; margin-left: 0.5rem; }"
  , ".container { max-width: 1200px; margin: 2rem auto; padding: 0 1rem; }"
  , ".search-bar { width: 100%; padding: 1rem; font-size: 1rem; border: 1px solid #ddd; border-radius: 0.5rem; margin-bottom: 2rem; }"
  , ".product-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(250px, 1fr)); gap: 2rem; margin-bottom: 2rem; }"
  , ".product-card { background: white; border-radius: 0.5rem; overflow: hidden; box-shadow: 0 2px 8px rgba(0,0,0,0.1); transition: transform 0.3s; }"
  , ".product-card:hover { transform: translateY(-4px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }"
  , ".product-image { width: 100%; height: 200px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); display: flex; align-items: center; justify-content: center; color: white; font-size: 3rem; }"
  , ".product-info { padding: 1rem; }"
  , ".product-name { font-size: 1.125rem; font-weight: 600; margin-bottom: 0.5rem; color: #2c3e50; }"
  , ".product-category { font-size: 0.875rem; color: #7f8c8d; margin-bottom: 0.5rem; }"
  , ".product-price { font-size: 1.25rem; font-weight: bold; color: #27ae60; margin-bottom: 1rem; }"
  , ".add-to-cart { width: 100%; padding: 0.75rem; background: #3498db; color: white; border: none; border-radius: 0.25rem; cursor: pointer; font-size: 1rem; transition: background 0.3s; }"
  , ".add-to-cart:hover { background: #2980b9; }"
  , ".cart-panel { position: fixed; right: -400px; top: 0; width: 400px; height: 100vh; background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.1); transition: right 0.3s; z-index: 1000; display: flex; flex-direction: column; }"
  , ".cart-panel.open { right: 0; }"
  , ".cart-header { padding: 1rem; background: #2c3e50; color: white; display: flex; justify-content: space-between; align-items: center; }"
  , ".cart-actions { display: flex; gap: 0.5rem; align-items: center; }"
  , ".cart-close { cursor: pointer; font-size: 1.5rem; }"
  , ".clear-cart { padding: 0.25rem 0.5rem; border: 1px solid #ecf0f1; border-radius: 0.25rem; background: white; cursor: pointer; }"
  , ".clear-cart:hover { background: #f8f9fa; }"
  , ".cart-items { flex: 1; overflow-y: auto; padding: 1rem; }"
  , ".cart-item { display: flex; justify-content: space-between; align-items: center; padding: 1rem; border-bottom: 1px solid #ecf0f1; }"
  , ".cart-item-info { flex: 1; }"
  , ".cart-item-name { font-weight: 600; margin-bottom: 0.25rem; }"
  , ".cart-item-price { color: #27ae60; }"
  , ".cart-item-quantity { display: flex; align-items: center; gap: 0.5rem; }"
  , ".quantity-btn { width: 30px; height: 30px; border: 1px solid #ddd; background: white; cursor: pointer; border-radius: 0.25rem; }"
  , ".quantity-btn:hover { background: #f8f9fa; }"
  , ".cart-footer { padding: 1rem; border-top: 2px solid #ecf0f1; }"
  , ".cart-total { display: flex; justify-content: space-between; font-size: 1.25rem; font-weight: bold; margin-bottom: 1rem; }"
  , ".checkout-btn { width: 100%; padding: 1rem; background: #27ae60; color: white; border: none; border-radius: 0.25rem; cursor: pointer; font-size: 1.125rem; font-weight: bold; }"
  , ".checkout-btn:hover { background: #229954; }"
  , ".empty-cart { text-align: center; padding: 2rem; color: #7f8c8d; }"
  , ".filter-bar { display: flex; gap: 1rem; margin-bottom: 2rem; flex-wrap: wrap; }"
  , ".filter-btn { padding: 0.5rem 1rem; background: white; border: 1px solid #ddd; border-radius: 0.25rem; cursor: pointer; transition: all 0.3s; }"
  , ".filter-btn:hover { background: #f8f9fa; }"
  , ".filter-btn.active { background: #3498db; color: white; border-color: #3498db; }"
  ]

-- Product type
data Product = Product
  { productId       :: Int
  , productName     :: Text
  , productCategory :: Text
  , productPrice    :: Double
  , productIcon     :: Text
  } deriving (Eq, Show)

-- Cart item type
data CartItem = CartItem
  { cartProduct  :: Product
  , cartQuantity :: Int
  } deriving (Eq, Show)

-- Sample products
sampleProducts :: [Product]
sampleProducts =
  [ Product 1 "Wireless Headphones" "Electronics" 79.99 "🎧"
  , Product 2 "Smart Watch" "Electronics" 199.99 "⌚"
  , Product 3 "Laptop Stand" "Accessories" 49.99 "💻"
  , Product 4 "USB-C Cable" "Accessories" 19.99 "🔌"
  , Product 5 "Mechanical Keyboard" "Electronics" 129.99 "⌨️"
  , Product 6 "Wireless Mouse" "Electronics" 39.99 "🖱️"
  , Product 7 "Monitor Light Bar" "Accessories" 89.99 "💡"
  , Product 8 "Desk Mat" "Accessories" 29.99 "🗂️"
  , Product 9 "Webcam" "Electronics" 69.99 "📷"
  , Product 10 "Phone Stand" "Accessories" 15.99 "📱"
  , Product 11 "Portable SSD" "Storage" 119.99 "💾"
  , Product 12 "SD Card Reader" "Storage" 24.99 "🗃️"
  ]

-- Helpers
tshow :: Show a => a -> Text
tshow = T.pack . show

money :: Double -> Text
money x = T.pack (printf "$%.2f" x)

-- Main app
app :: MonadWidget t m => m ()
app = mdo
  -- Header with cart toggle
  cartToggle <- elAttr "div" ("class" =: "header") $ do
    elAttr "div" ("class" =: "header-content") $ do
      elAttr "div" ("class" =: "logo") $ text "🛍️ Reflex Store"
      elAttr "div" ("class" =: "nav") $ do
        elAttr "span" ("class" =: "nav-item") $ text "Products"
        elAttr "span" ("class" =: "nav-item") $ text "Categories"
        (cartBtn, _) <- elAttr' "span" ("class" =: "nav-item") $ do
          text "🛒 Cart"
          let cartCount = ffor cartDyn $ \cart ->
                sum $ map cartQuantity $ Map.elems cart
          dyn_ $ ffor cartCount $ \count ->
            when (count > 0) $
              elAttr "span" ("class" =: "cart-badge") $ text $ tshow count
        return $ domEvent Click cartBtn

  -- Cart state
  cartDyn <- foldDyn ($) Map.empty cartUpdates

  -- Main container
  addToCartEvents <- elAttr "div" ("class" =: "container") $ do
    -- Search bar
    searchInput <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("class" =: "search-bar" <> "placeholder" =: "Search products...")

    let searchTermRaw = _inputElement_value searchInput
        searchTerm    = T.toLower . T.strip <$> searchTermRaw

    -- Category filter
    selectedCategory <- elAttr "div" ("class" =: "filter-bar") $ do
      let categories = "All" : ["Electronics", "Accessories", "Storage"]
      rec
        categoryEvents <- forM categories $ \cat -> do
          let isActive = ffor selectedCat $ (== cat)
              attrs = ffor isActive $ \active ->
                "class" =: if active then "filter-btn active" else "filter-btn"
          (btn, _) <- elDynAttr' "button" attrs $ text cat
          return (cat <$ domEvent Click btn)
        selectedCat <- holdDyn "All" $ leftmost categoryEvents
      return selectedCat

    -- Filtered products
    let filteredProducts = ffor2 searchTerm selectedCategory $ \search cat ->
          filter (\p ->
            (T.null search ||
               T.isInfixOf search (T.toLower $ productName p)) &&
            (cat == "All" || productCategory p == cat)
          ) sampleProducts

    -- Product grid
    elAttr "div" ("class" =: "product-grid") $ do
      productEvents <- dyn $ ffor filteredProducts $ \prods -> do
        events <- forM prods $ \product -> productCard' product
        return $ leftmost events
      switchHold never productEvents

  -- Cart panel (returns update functions)
  cartPanelUpdates <- cartPanel cartToggle cartDyn

  -- Combine all cart updates
  let cartUpdates = mergeWith (.)
        [ ffor addToCartEvents $ \p ->
            Map.insertWith
              (\_new old -> old { cartQuantity = cartQuantity old + 1 })
              (productId p) (CartItem p 1)
        , cartPanelUpdates
        ]

  return ()

-- Modified product card that returns events
productCard' :: MonadWidget t m => Product -> m (Event t Product)
productCard' product = do
  elAttr "div" ("class" =: "product-card") $ do
    elAttr "div" ("class" =: "product-image") $
      text $ productIcon product
    elAttr "div" ("class" =: "product-info") $ do
      elAttr "div" ("class" =: "product-name") $ text $ productName product
      elAttr "div" ("class" =: "product-category") $ text $ productCategory product
      elAttr "div" ("class" =: "product-price") $
        text $ money (productPrice product)
      (addBtn, _) <- elAttr' "button" ("class" =: "add-to-cart") $ text "Add to Cart"
      return $ product <$ domEvent Click addBtn

-- Cart panel widget
cartPanel
  :: MonadWidget t m
  => Event t ()                                -- ^ toggle (header cart icon)
  -> Dynamic t (Map Int CartItem)              -- ^ cart state to display
  -> m (Event t (Map Int CartItem -> Map Int CartItem)) -- ^ update functions
cartPanel toggleEvent cartDyn = mdo
  -- Open/close state (wired to both toggle and close)
  isOpenDyn <- foldDyn ($) False (leftmost [not <$ toggleEvent, const False <$ closeClick])

  let panelAttrs = ffor isOpenDyn $ \isOpen ->
        "class" =: if isOpen then "cart-panel open" else "cart-panel"

  (closeClick, updates) <- elDynAttr "div" panelAttrs $ do
    -- Cart header
    (closeClickEv, clearCartUpdateEv) <- elAttr "div" ("class" =: "cart-header") $ do
      el "h2" $ text "Shopping Cart"
      -- Close + clear buttons
      elAttr "div" ("class" =: "cart-actions") $ do
        (clearBtn, _) <- elAttr' "button" ("class" =: "clear-cart") $ text "Clear"
        (closeBtn, _) <- elAttr' "span" ("class" =: "cart-close") $ text "×"
        -- Emit updates: clear -> Map.empty; close handled by isOpenDyn above
        let clearEvent = constMap Map.empty <$ domEvent Click clearBtn
        -- Return both the close click event and the clear update event
        pure (domEvent Click closeBtn, clearEvent)

    -- Cart items
    cartItemEvents <- elAttr "div" ("class" =: "cart-items") $ do
      itemEvents <- dyn $ ffor cartDyn $ \cart ->
        if Map.null cart
          then elAttr "div" ("class" =: "empty-cart") $ do
                 el "p" $ text "Your cart is empty"
                 el "p" $ text "Add some products to get started!"
                 return never
          else do
            events <- forM (Map.toList cart) $ \(_pid, item) ->
              cartItemWidget item
            return $ leftmost events
      switchHold never itemEvents

    -- Cart footer
    footerUpdateEv <- elAttr "div" ("class" =: "cart-footer") $ do
      elAttr "div" ("class" =: "cart-total") $ do
        el "span" $ text "Total:"
        dynText $ ffor cartDyn $ \cart ->
          let total = sum [productPrice (cartProduct item) * fromIntegral (cartQuantity item)
                          | item <- Map.elems cart]
          in money total
      (checkoutBtn, _) <- elAttr' "button" ("class" =: "checkout-btn") $ text "Checkout"
      -- Fixed: properly handle the performEvent_ with correct type
      performEvent_ $ ffor (domEvent Click checkoutBtn) $ \() -> pure ()
      pure never

    -- Merge all update events from items + clear
    -- Return both the close click event and the merged updates
    pure (closeClickEv, leftmost [cartItemEvents, clearCartUpdateEv, footerUpdateEv])

  -- Return only the cart updates
  pure updates
  where
    -- Small helper to satisfy type inference in the clear button:
    -- constMap x = const (const x) specialized for Map state update
    constMap :: Map Int CartItem -> (Map Int CartItem -> Map Int CartItem)
    constMap x = const x

-- Cart item widget
cartItemWidget
  :: MonadWidget t m
  => CartItem
  -> m (Event t (Map Int CartItem -> Map Int CartItem))
cartItemWidget item = do
  elAttr "div" ("class" =: "cart-item") $ do
    elAttr "div" ("class" =: "cart-item-info") $ do
      elAttr "div" ("class" =: "cart-item-name") $ text $ productName $ cartProduct item
      elAttr "div" ("class" =: "cart-item-price") $
        text $ T.pack $ printf "$%.2f × %d"
          (productPrice $ cartProduct item) (cartQuantity item)
    elAttr "div" ("class" =: "cart-item-quantity") $ do
      (minusBtn, _)  <- elAttr' "button" ("class" =: "quantity-btn") $ text "-"
      el "span" $ text $ tshow $ cartQuantity item
      (plusBtn, _)   <- elAttr' "button" ("class" =: "quantity-btn") $ text "+"
      (removeBtn, _) <- elAttr' "button" ("class" =: "quantity-btn") $ text "🗑️"

      let pid = productId $ cartProduct item
          minusEvent = Map.update (\i ->
            let newQty = cartQuantity i - 1
            in if newQty > 0 then Just i { cartQuantity = newQty } else Nothing) pid
            <$ domEvent Click minusBtn
          plusEvent = Map.update (\i -> Just i { cartQuantity = cartQuantity i + 1 }) pid
            <$ domEvent Click plusBtn
          removeEvent = Map.delete pid <$ domEvent Click removeBtn

      return $ leftmost [minusEvent, plusEvent, removeEvent]
