-- | Wire types shared between the directo backend and frontends.
--
-- Every type here crosses the HTTP boundary as JSON, so both ends must
-- be compiled against the same version of this module (guaranteed by the
-- shared package). Plain 'Generic' deriving only — no TH — so the module
-- builds identically on native GHC and the JS backend.
module Directo.Types
  ( Category(..)
  , allCategories
  , categoryLabel
  , categoryFromText
  , Product(..)
  , ProductInput(..)
  , CartItem(..)
  , ShippingAddress(..)
  , CheckoutReq(..)
  , CheckoutResp(..)
  , OrderStatus(..)
  , orderStatusLabel
  , orderStatusFromText
  , orderStatusToText
  , OrderStatusResp(..)
  , OrderSummary(..)
  , OrderLine(..)
  , OrderDetail(..)
  , StoreAddress(..)
  , GuiaDhl(..)
  , LoginRequest(..)
  , ShipReq(..)
  , UserInfo(..)
  , AuthConfig(..)
  , ContactReq(..)
  , ContactMessage(..)
  , CustomerSummary(..)
  , centsToMxn
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Int     (Int64)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

-- | The nine catalog categories of the Querétaro store.
data Category
  = Lavadoras
  | Secadoras
  | Refrigeradores
  | Licuadoras
  | Cocina
  | AiresAcondicionados
  | Extractores
  | MotoresYBombas
  | Otros
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

allCategories :: [Category]
allCategories = [minBound .. maxBound]

categoryLabel :: Category -> Text
categoryLabel = \case
  Lavadoras           -> "Lavadoras"
  Secadoras           -> "Secadoras"
  Refrigeradores      -> "Refrigeradores"
  Licuadoras          -> "Licuadoras"
  Cocina              -> "Cocina"
  AiresAcondicionados -> "Aires Acondicionados"
  Extractores         -> "Extractores"
  MotoresYBombas      -> "Motores y Bombas"
  Otros               -> "Otros"

-- | Inverse of 'categoryLabel' (also accepts the constructor name, which
-- is what the database column stores).
categoryFromText :: Text -> Maybe Category
categoryFromText t =
  lookup t [ (categoryLabel c, c) | c <- allCategories ]
  `orElse` lookup t [ (T.pack (show c), c) | c <- allCategories ]
  where orElse (Just x) _ = Just x
        orElse Nothing  y = y

data Product = Product
  { productId   :: Int64
  , sku         :: Text
  , slug        :: Text       -- ^ URL identity, e.g. "bomba-de-desague-whirlpool-vmw"
  , name        :: Text
  , description :: Text
  , category    :: Category
  , brand       :: Text
  , priceCents  :: Int        -- ^ MXN centavos
  , stock       :: Int
  , weightGrams :: Maybe Int
  , imageUrl    :: Maybe Text
  , active      :: Bool
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Admin create/update payload ('productId' is taken from the URL).
data ProductInput = ProductInput
  { piSku         :: Text
  , piSlug        :: Text
  , piName        :: Text
  , piDescription :: Text
  , piCategory    :: Category
  , piBrand       :: Text
  , piPriceCents  :: Int
  , piStock       :: Int
  , piWeightGrams :: Maybe Int
  , piImageUrl    :: Maybe Text
  , piActive      :: Bool
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CartItem = CartItem
  { ciProductId :: Int64
  , ciQuantity  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ShippingAddress = ShippingAddress
  { calle        :: Text
  , numero       :: Text
  , colonia      :: Text
  , codigoPostal :: Text
  , ciudad       :: Text
  , estado       :: Text
  , referencias  :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CheckoutReq = CheckoutReq
  { customerName  :: Text
  , customerEmail :: Text
  , customerPhone :: Text
  , address       :: ShippingAddress
  , items         :: [CartItem]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CheckoutResp = CheckoutResp
  { orderRef  :: Text
  , initPoint :: Maybe Text  -- ^ Mercado Pago redirect URL; Nothing when MP is disabled (dev)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderStatus
  = Created
  | PendingPayment
  | Paid
  | Shipped
  | Completed
  | Cancelled
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

orderStatusLabel :: OrderStatus -> Text
orderStatusLabel = \case
  Created        -> "Creado"
  PendingPayment -> "Pago pendiente"
  Paid           -> "Pagado"
  Shipped        -> "Enviado"
  Completed      -> "Completado"
  Cancelled      -> "Cancelado"

-- | The database representation ('created', 'pending_payment', …).
orderStatusToText :: OrderStatus -> Text
orderStatusToText = \case
  Created        -> "created"
  PendingPayment -> "pending_payment"
  Paid           -> "paid"
  Shipped        -> "shipped"
  Completed      -> "completed"
  Cancelled      -> "cancelled"

orderStatusFromText :: Text -> Maybe OrderStatus
orderStatusFromText t =
  lookup t [ (orderStatusToText s, s) | s <- [minBound .. maxBound] ]

data OrderStatusResp = OrderStatusResp
  { osrRef    :: Text
  , osrStatus :: OrderStatus
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderSummary = OrderSummary
  { osId         :: Int64
  , osRef        :: Text
  , osStatus     :: OrderStatus
  , osCustomer   :: Text
  , osTotalCents :: Int
  , osWaybill    :: Maybe Text
  , osCreatedAt  :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderLine = OrderLine
  { olProductName    :: Text
  , olProductSku     :: Text
  , olQuantity       :: Int
  , olUnitPriceCents :: Int
  , olWeightGrams    :: Maybe Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderDetail = OrderDetail
  { odSummary   :: OrderSummary
  , odEmail     :: Text
  , odPhone     :: Text
  , odAddress   :: ShippingAddress
  , odLines     :: [OrderLine]
  , odMpStatus  :: Maybe Text
  , odMpPayment :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Sender block for the DHL sheet (the physical store in Querétaro).
data StoreAddress = StoreAddress
  { saName         :: Text
  , saAddressLines :: [Text]
  , saPhone        :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Everything DHL asks for at the counter (or in MyDHL) for one order.
data GuiaDhl = GuiaDhl
  { gReference           :: Text
  , gSender              :: StoreAddress
  , gRecipientName       :: Text
  , gRecipientPhone      :: Text
  , gRecipientAddress    :: ShippingAddress
  , gItems               :: [OrderLine]
  , gTotalWeightGrams    :: Int
  , gDeclaredValueCents  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype LoginRequest = LoginRequest
  { lrPassword :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON LoginRequest
instance FromJSON LoginRequest

newtype ShipReq = ShipReq
  { srWaybill :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ShipReq
instance FromJSON ShipReq

-- | The signed-in customer, as exposed to the frontend.
data UserInfo = UserInfo
  { uiName    :: Text
  , uiEmail   :: Text
  , uiPicture :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Which social-login providers the backend has credentials for.
newtype AuthConfig = AuthConfig
  { acProviders :: [Text]   -- ^ e.g. ["google"]
  } deriving (Show, Eq, Generic)

instance ToJSON AuthConfig
instance FromJSON AuthConfig

-- | A submission of the public Contacto form.
data ContactReq = ContactReq
  { crName    :: Text
  , crEmail   :: Text
  , crPhone   :: Text
  , crMessage :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A stored contact message, as listed in the admin inbox.
data ContactMessage = ContactMessage
  { cmId        :: Int64
  , cmName      :: Text
  , cmEmail     :: Text
  , cmPhone     :: Text
  , cmMessage   :: Text
  , cmCreatedAt :: UTCTime
  , cmReadAt    :: Maybe UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | One row of the admin Clientes view: a customer aggregated from their
-- orders (keyed by email; they may or may not have a Google account).
data CustomerSummary = CustomerSummary
  { csEmail           :: Text
  , csName            :: Text
  , csPhone           :: Text
  , csHasAccount      :: Bool
  , csOrderCount      :: Int
  , csTotalSpentCents :: Int
  , csLastOrderAt     :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Render centavos as \"$1,234.50\".
centsToMxn :: Int -> Text
centsToMxn cents =
  let (pesos, cts) = cents `divMod` 100
      -- group thousands from the right: reverse, chunk, un-reverse each chunk
      grouped = T.intercalate "," (map T.reverse (reverse (T.chunksOf 3 (T.reverse (T.pack (show pesos))))))
      pad2 n = let s = show n in if n < 10 then '0' : s else s
  in "$" <> grouped <> "." <> T.pack (pad2 cts)
