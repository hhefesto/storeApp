module Db
  ( initDb
  , listProducts
  , listAllProducts
  , insertProduct
  , updateProduct
  , deactivateProduct
  , createOrder
  , OrderError(..)
  , getOrderStatusByRef
  , getOrderIdByRef
  , listOrders
  , getOrderDetail
  , setPreference
  , setStripeSession
  , paymentInfo
  , getOrderIdByStripeSession
  , recordPayment
  , transition
  , setWaybill
  , guiaFor
  , insertSession
  , sessionExists
  , deleteSession
  , mpCheckDue
  , upsertUser
  , insertUserSession
  , userForSession
  , deleteUserSession
  , attachOrderUser
  , listOrdersForUser
  , insertContactMessage
  , listContactMessages
  , markMessageRead
  , listCustomers
  , listOrdersForEmail
  ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Monad              (forM, forM_, void)
import qualified Data.ByteString.Char8      as BC8
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           System.Environment         (lookupEnv)

import           Directo.Types
import qualified OrderRef

newtype MissingEnv = MissingEnv String deriving Show
instance Exception MissingEnv

initDb :: IO Connection
initDb = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> connectPostgreSQL (BC8.pack url)
    Nothing  -> throwIO (MissingEnv "DATABASE_URL")

-- ── products ────────────────────────────────────────────────────────────

type ProductRow =
  (Int64, Text, Text, Text, Text, Text, Text, Int, Int, Maybe Int, Maybe Text, Bool)

rowToProduct :: ProductRow -> Product
rowToProduct (pid, psku, pslug, pname, pdesc, pcat, pbrand, pprice, pstock, pweight, pimg, pactive) =
  Product { productId   = pid
          , sku         = psku
          , slug        = pslug
          , name        = pname
          , description = pdesc
          , category    = fromMaybe Otros (categoryFromText pcat)
          , brand       = pbrand
          , priceCents  = pprice
          , stock       = pstock
          , weightGrams = pweight
          , imageUrl    = pimg
          , active      = pactive
          }

productColumns :: Query
productColumns =
  "id, sku, slug, name, description, category, brand, price_cents, stock, weight_grams, image_url, active"

listProducts :: Connection -> IO [Product]
listProducts conn = map rowToProduct <$> query_ conn
  ("SELECT " <> productColumns <> " FROM products WHERE active ORDER BY name")

listAllProducts :: Connection -> IO [Product]
listAllProducts conn = map rowToProduct <$> query_ conn
  ("SELECT " <> productColumns <> " FROM products ORDER BY name")

insertProduct :: Connection -> ProductInput -> IO Product
insertProduct conn p = do
  [row] <- query conn
    ("INSERT INTO products (sku, slug, name, description, category, brand, price_cents, stock, weight_grams, image_url, active) \
     \VALUES (?,?,?,?,?,?,?,?,?,?,?) RETURNING " <> productColumns)
    ( piSku p, piSlug p, piName p, piDescription p, T.pack (show (piCategory p)), piBrand p
    , piPriceCents p, piStock p, piWeightGrams p, piImageUrl p, piActive p )
  pure (rowToProduct row)

updateProduct :: Connection -> Int64 -> ProductInput -> IO (Maybe Product)
updateProduct conn pid p = do
  rows <- query conn
    ("UPDATE products SET sku=?, slug=?, name=?, description=?, category=?, brand=?, price_cents=?, \
     \stock=?, weight_grams=?, image_url=?, active=?, updated_at=NOW() \
     \WHERE id=? RETURNING " <> productColumns)
    ( piSku p, piSlug p, piName p, piDescription p, T.pack (show (piCategory p)), piBrand p
    , piPriceCents p, piStock p, piWeightGrams p, piImageUrl p, piActive p, pid )
  pure (rowToProduct <$> listToMaybe rows)

deactivateProduct :: Connection -> Int64 -> IO ()
deactivateProduct conn pid = void $ execute conn
  "UPDATE products SET active = FALSE, updated_at = NOW() WHERE id = ?" (Only pid)

-- ── orders ──────────────────────────────────────────────────────────────

data OrderError
  = EmptyCart
  | UnknownProduct Int64
  | OutOfStock Text        -- ^ product name
  deriving Show
instance Exception OrderError

-- | Validate the cart, snapshot prices, and insert the order atomically.
-- Stock is checked (not reserved); it is decremented on the Paid transition.
createOrder :: Connection -> CheckoutReq -> IO (Int64, Text, Int)
createOrder conn req = withTransaction conn $ do
  case items req of
    [] -> throwIO EmptyCart
    _  -> pure ()
  linesAndTotals <- forM (items req) $ \ci -> do
    rows <- query conn
      ("SELECT " <> productColumns <> " FROM products WHERE id = ? AND active")
      (Only (ciProductId ci))
    case map rowToProduct rows of
      [] -> throwIO (UnknownProduct (ciProductId ci))
      (p:_) -> do
        if stock p < ciQuantity ci
          then throwIO (OutOfStock (name p))
          else pure (p, ciQuantity ci)
  let total = sum [ priceCents p * q | (p, q) <- linesAndTotals ]
      addr  = address req
  ref <- OrderRef.newOrderRef conn
  [Only oid] <- query conn
    "INSERT INTO orders (order_ref, status, customer_name, customer_email, customer_phone, \
    \calle, numero, colonia, codigo_postal, ciudad, estado, referencias, total_cents) \
    \VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?) RETURNING id"
    ( ref, orderStatusToText Created
    , customerName req, customerEmail req, customerPhone req
    , calle addr, numero addr, colonia addr, codigoPostal addr
    , ciudad addr, estado addr, referencias addr, total )
  forM_ linesAndTotals $ \(p, q) -> execute conn
    "INSERT INTO order_items (order_id, product_id, quantity, unit_price_cents, product_name, product_sku, weight_grams) \
    \VALUES (?,?,?,?,?,?,?)"
    (oid :: Int64, productId p, q, priceCents p, name p, sku p, weightGrams p)
  void $ execute conn
    "INSERT INTO order_events (order_id, from_status, to_status, note) VALUES (?, NULL, ?, 'pedido creado')"
    (oid, orderStatusToText Created)
  pure (oid, ref, total)

getOrderStatusByRef :: Connection -> Text -> IO (Maybe OrderStatus)
getOrderStatusByRef conn ref = do
  rows <- query conn "SELECT status FROM orders WHERE order_ref = ?" (Only ref)
  pure $ listToMaybe [ s | Only st <- rows, Just s <- [orderStatusFromText st] ]

getOrderIdByRef :: Connection -> Text -> IO (Maybe Int64)
getOrderIdByRef conn ref = do
  rows <- query conn "SELECT id FROM orders WHERE order_ref = ?" (Only ref)
  pure $ listToMaybe [ oid | Only oid <- rows ]

type OrderSummaryRow = (Int64, Text, Text, Text, Int, Maybe Text, UTCTime)

rowToSummary :: OrderSummaryRow -> OrderSummary
rowToSummary (oid, ref, st, customer, total, waybill, created) =
  OrderSummary { osId = oid, osRef = ref
               , osStatus = fromMaybe Created (orderStatusFromText st)
               , osCustomer = customer, osTotalCents = total
               , osWaybill = waybill, osCreatedAt = created }

summaryColumns :: Query
summaryColumns = "id, order_ref, status, customer_name, total_cents, dhl_waybill, created_at"

listOrders :: Connection -> Maybe OrderStatus -> IO [OrderSummary]
listOrders conn mStatus = map rowToSummary <$> case mStatus of
  Nothing -> query_ conn
    ("SELECT " <> summaryColumns <> " FROM orders ORDER BY created_at DESC")
  Just st -> query conn
    ("SELECT " <> summaryColumns <> " FROM orders WHERE status = ? ORDER BY created_at DESC")
    (Only (orderStatusToText st))

getOrderDetail :: Connection -> Int64 -> IO (Maybe OrderDetail)
getOrderDetail conn oid = do
  rows <- query conn
    ("SELECT " <> summaryColumns <> ", customer_email, customer_phone, \
     \calle, numero, colonia, codigo_postal, ciudad, estado, referencias, mp_status, mp_payment_id \
     \FROM orders WHERE id = ?")
    (Only oid)
  case rows of
    [] -> pure Nothing
    (( soid, ref, st, customer, total, waybill, created
     , email, phone, c, n, col, cp, ciu, est, refs, mpSt, mpPay ):_) -> do
      lineRows <- query conn
        "SELECT product_name, product_sku, quantity, unit_price_cents, weight_grams \
        \FROM order_items WHERE order_id = ? ORDER BY product_name"
        (Only oid)
      let toLine (lname, lsku, q, unit, w) = OrderLine lname lsku q unit w
      pure . Just $ OrderDetail
        { odSummary   = rowToSummary (soid, ref, st, customer, total, waybill, created)
        , odEmail     = email
        , odPhone     = phone
        , odAddress   = ShippingAddress c n col cp ciu est refs
        , odLines     = map toLine lineRows
        , odMpStatus  = mpSt
        , odMpPayment = mpPay
        }

setPreference :: Connection -> Int64 -> Text -> IO ()
setPreference conn oid prefId = void $ execute conn
  "UPDATE orders SET mp_preference_id = ?, payment_provider = 'mercadopago', updated_at = NOW() WHERE id = ?"
  (prefId, oid)

setStripeSession :: Connection -> Int64 -> Text -> IO ()
setStripeSession conn oid sessionId = void $ execute conn
  "UPDATE orders SET stripe_session_id = ?, payment_provider = 'stripe', updated_at = NOW() WHERE id = ?"
  (sessionId, oid)

-- | Which provider (and provider-side id) an order is being paid with.
paymentInfo :: Connection -> Int64 -> IO (Maybe (Maybe Text, Maybe Text))
paymentInfo conn oid = do
  rows <- query conn
    "SELECT payment_provider, stripe_session_id FROM orders WHERE id = ?" (Only oid)
  pure (listToMaybe rows)

-- | Resolve a Stripe Checkout Session back to our order.
getOrderIdByStripeSession :: Connection -> Text -> IO (Maybe Int64)
getOrderIdByStripeSession conn sessionId = do
  rows <- query conn
    "SELECT id FROM orders WHERE stripe_session_id = ?" (Only sessionId)
  pure $ listToMaybe [ oid | Only oid <- rows ]

recordPayment :: Connection -> Int64 -> Text -> Text -> IO ()
recordPayment conn oid paymentId mpStatus = void $ execute conn
  "UPDATE orders SET mp_payment_id = ?, mp_status = ?, updated_at = NOW() WHERE id = ?"
  (paymentId, mpStatus, oid)

-- | Rate-limit Mercado Pago polling: returns True (and stamps the check)
-- at most once every @seconds@ per order.
mpCheckDue :: Connection -> Int64 -> Int -> IO Bool
mpCheckDue conn oid seconds = do
  n <- execute conn
    "UPDATE orders SET mp_last_check = NOW() WHERE id = ? \
    \AND (mp_last_check IS NULL OR mp_last_check < NOW() - make_interval(secs => ?))"
    (oid, seconds)
  pure (n > 0)

-- ── state machine ───────────────────────────────────────────────────────

legalTransitions :: [(OrderStatus, OrderStatus)]
legalTransitions =
  [ (Created,        PendingPayment)
  , (Created,        Paid)            -- dev mode / manual charge
  , (Created,        Cancelled)
  , (PendingPayment, Paid)
  , (PendingPayment, Cancelled)
  , (Paid,           Shipped)
  , (Paid,           Cancelled)
  , (Shipped,        Completed)
  ]

-- | Compare-and-swap transition. On entry to Paid, decrement stock for the
-- order's items in the same transaction (floored: a real payment is never
-- blocked by a stock discrepancy; the shortfall is recorded as an event).
transition :: Connection -> Int64 -> OrderStatus -> Maybe Text -> IO Bool
transition conn oid to note = withTransaction conn $ do
  rows <- query conn "SELECT status FROM orders WHERE id = ? FOR UPDATE" (Only oid)
  case [ s | Only st <- rows, Just s <- [orderStatusFromText st] ] of
    [from] | (from, to) `elem` legalTransitions -> do
      void $ execute conn
        "UPDATE orders SET status = ?, updated_at = NOW() WHERE id = ?"
        (orderStatusToText to, oid)
      void $ execute conn
        "INSERT INTO order_events (order_id, from_status, to_status, note) VALUES (?,?,?,?)"
        (oid, orderStatusToText from, orderStatusToText to, note)
      case to of
        Paid -> decrementStock conn oid
        _    -> pure ()
      pure True
    _ -> pure False

decrementStock :: Connection -> Int64 -> IO ()
decrementStock conn oid = do
  itemRows <- query conn
    "SELECT product_id, quantity, product_name FROM order_items WHERE order_id = ?"
    (Only oid)
  forM_ itemRows $ \(pid :: Int64, q :: Int, pname :: Text) -> do
    n <- execute conn
      "UPDATE products SET stock = stock - ?, updated_at = NOW() WHERE id = ? AND stock >= ?"
      (q, pid, q)
    if n > 0
      then pure ()
      else do
        -- not enough stock: floor at zero and leave an audit note
        void $ execute conn
          "UPDATE products SET stock = 0, updated_at = NOW() WHERE id = ?" (Only pid)
        void $ execute conn
          "INSERT INTO order_events (order_id, from_status, to_status, note) VALUES (?, NULL, ?, ?)"
          (oid, orderStatusToText Paid, "stock insuficiente: " <> pname)

setWaybill :: Connection -> Int64 -> Text -> IO ()
setWaybill conn oid waybill = void $ execute conn
  "UPDATE orders SET dhl_waybill = ?, updated_at = NOW() WHERE id = ?" (waybill, oid)

-- ── guía DHL ────────────────────────────────────────────────────────────

-- | Assemble the DHL sheet for an order. Items without a recorded weight
-- are estimated at @fallbackGrams@.
guiaFor :: Connection -> StoreAddress -> Int -> Int64 -> IO (Maybe GuiaDhl)
guiaFor conn sender fallbackGrams oid = do
  mDetail <- getOrderDetail conn oid
  pure $ flip fmap mDetail $ \d ->
    let totalW = sum [ fromMaybe fallbackGrams (olWeightGrams l) * olQuantity l
                     | l <- odLines d ]
    in GuiaDhl
      { gReference          = osRef (odSummary d)
      , gSender             = sender
      , gRecipientName      = osCustomer (odSummary d)
      , gRecipientPhone     = odPhone d
      , gRecipientAddress   = odAddress d
      , gItems              = odLines d
      , gTotalWeightGrams   = totalW
      , gDeclaredValueCents = osTotalCents (odSummary d)
      }

-- ── admin sessions ──────────────────────────────────────────────────────

insertSession :: Connection -> Text -> IO ()
insertSession conn token = void $ execute conn
  "INSERT INTO admin_sessions (token) VALUES (?)" (Only token)

sessionExists :: Connection -> Text -> IO Bool
sessionExists conn token = do
  rows <- query conn
    "SELECT 1::int FROM admin_sessions WHERE token = ? AND created_at > NOW() - interval '24 hours'"
    (Only token)
  pure (not (null (rows :: [Only Int])))

deleteSession :: Connection -> Text -> IO ()
deleteSession conn token = void $ execute conn
  "DELETE FROM admin_sessions WHERE token = ?" (Only token)

-- ── customer accounts (social login) ────────────────────────────────────

-- | Insert or refresh a user identified by (provider, subject); returns id.
upsertUser :: Connection -> Text -> Text -> Text -> Text -> Maybe Text -> IO Int64
upsertUser conn provider subject email uname picture = do
  [Only uid] <- query conn
    "INSERT INTO users (provider, provider_subject, email, name, picture) \
    \VALUES (?,?,?,?,?) \
    \ON CONFLICT (provider, provider_subject) \
    \DO UPDATE SET email = EXCLUDED.email, name = EXCLUDED.name, picture = EXCLUDED.picture \
    \RETURNING id"
    (provider, subject, email, uname, picture)
  pure uid

insertUserSession :: Connection -> Text -> Int64 -> IO ()
insertUserSession conn token uid = void $ execute conn
  "INSERT INTO user_sessions (token, user_id) VALUES (?, ?)" (token, uid)

-- | Resolve a (30-day) customer session to (userId, info).
userForSession :: Connection -> Text -> IO (Maybe (Int64, UserInfo))
userForSession conn token = do
  rows <- query conn
    "SELECT u.id, u.name, u.email, u.picture FROM user_sessions s \
    \JOIN users u ON u.id = s.user_id \
    \WHERE s.token = ? AND s.created_at > NOW() - interval '30 days'"
    (Only token)
  pure $ listToMaybe
    [ (uid, UserInfo uname email picture)
    | (uid, uname, email, picture) <- rows ]

deleteUserSession :: Connection -> Text -> IO ()
deleteUserSession conn token = void $ execute conn
  "DELETE FROM user_sessions WHERE token = ?" (Only token)

attachOrderUser :: Connection -> Int64 -> Int64 -> IO ()
attachOrderUser conn oid uid = void $ execute conn
  "UPDATE orders SET user_id = ? WHERE id = ?" (uid, oid)

listOrdersForUser :: Connection -> Int64 -> IO [OrderSummary]
listOrdersForUser conn uid = map rowToSummary <$> query conn
  ("SELECT " <> summaryColumns <> " FROM orders WHERE user_id = ? ORDER BY created_at DESC")
  (Only uid)

-- ── contact messages ────────────────────────────────────────────────────

insertContactMessage :: Connection -> ContactReq -> IO ()
insertContactMessage conn cr = void $ execute conn
  "INSERT INTO contact_messages (name, email, phone, message) VALUES (?,?,?,?)"
  (crName cr, crEmail cr, crPhone cr, crMessage cr)

listContactMessages :: Connection -> IO [ContactMessage]
listContactMessages conn = do
  rows <- query_ conn
    "SELECT id, name, email, phone, message, created_at, read_at \
    \FROM contact_messages ORDER BY created_at DESC LIMIT 500"
  pure [ ContactMessage i n e p m c r | (i, n, e, p, m, c, r) <- rows ]

markMessageRead :: Connection -> Int64 -> IO ()
markMessageRead conn mid = void $ execute conn
  "UPDATE contact_messages SET read_at = NOW() WHERE id = ? AND read_at IS NULL"
  (Only mid)

-- ── customers (admin view) ──────────────────────────────────────────────

-- | Customers aggregated from their orders, keyed by email. Cancelled
-- orders count toward history but not toward money spent.
listCustomers :: Connection -> IO [CustomerSummary]
listCustomers conn = do
  rows <- query_ conn
    "SELECT o.customer_email, \
    \       (array_agg(o.customer_name ORDER BY o.created_at DESC))[1], \
    \       (array_agg(o.customer_phone ORDER BY o.created_at DESC))[1], \
    \       bool_or(o.user_id IS NOT NULL), \
    \       count(*)::int, \
    \       coalesce(sum(o.total_cents) FILTER (WHERE o.status IN ('paid','shipped','completed')), 0)::int, \
    \       max(o.created_at) \
    \FROM orders o GROUP BY o.customer_email ORDER BY max(o.created_at) DESC"
  pure [ CustomerSummary email uname phone hasAcct cnt spent lastAt
       | (email, uname, phone, hasAcct, cnt, spent, lastAt) <- rows ]

listOrdersForEmail :: Connection -> Text -> IO [OrderSummary]
listOrdersForEmail conn email = map rowToSummary <$> query conn
  ("SELECT " <> summaryColumns <> " FROM orders WHERE customer_email = ? ORDER BY created_at DESC")
  (Only email)
