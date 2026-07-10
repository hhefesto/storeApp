-- | Minimal Mercado Pago Checkout Pro client.
--
-- Configured entirely through the environment:
--   MERCADOPAGO_ACCESS_TOKEN   — enables MP when present
--   MERCADOPAGO_WEBHOOK_SECRET — enables x-signature verification when present
--   DIRECTO_PUBLIC_BASE_URL    — back_urls / notification_url base
module MercadoPago
  ( MpConfig(..)
  , loadConfig
  , createPreference
  , fetchPaymentStatus
  , searchPaymentByRef
  , verifySignature
  ) where

import qualified Crypto.Hash.SHA256      as SHA256
import           Data.Aeson
import           Data.Aeson.Key          (Key)
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (newTlsManager)
import           System.Environment      (lookupEnv)

import           Directo.Types

data MpConfig = MpConfig
  { mpAccessToken   :: Maybe Text
  , mpWebhookSecret :: Maybe Text
  , mpPublicBaseUrl :: Text
  , mpManager       :: Manager
  }

loadConfig :: IO MpConfig
loadConfig = do
  token   <- fmap T.pack <$> lookupEnv "MERCADOPAGO_ACCESS_TOKEN"
  secret  <- fmap T.pack <$> lookupEnv "MERCADOPAGO_WEBHOOK_SECRET"
  baseUrl <- fromMaybe "http://localhost" . fmap T.pack
               <$> lookupEnv "DIRECTO_PUBLIC_BASE_URL"
  manager <- newTlsManager
  pure (MpConfig token secret baseUrl manager)

authedRequest :: Text -> String -> IO Request
authedRequest token url = do
  req <- parseRequest url
  pure req { requestHeaders =
               [ ("Authorization", "Bearer " <> TE.encodeUtf8 token)
               , ("Content-Type", "application/json")
               ] }

-- | Create a Checkout Pro preference; returns (preferenceId, initPoint).
createPreference :: MpConfig -> Text -> [OrderLine] -> IO (Maybe (Text, Text))
createPreference cfg ref orderLines = case mpAccessToken cfg of
  Nothing -> pure Nothing
  Just token -> do
    req0 <- authedRequest token "https://api.mercadopago.com/checkout/preferences"
    let base = mpPublicBaseUrl cfg
        backUrl st = base <> "/#/pago/" <> st <> "?ref=" <> ref
        body = object
          [ "items" .=
              [ object
                  [ "title"       .= olProductName l
                  , "quantity"    .= olQuantity l
                  , "unit_price"  .= (fromIntegral (olUnitPriceCents l) / 100 :: Double)
                  , "currency_id" .= ("MXN" :: Text)
                  ]
              | l <- orderLines ]
          , "external_reference" .= ref
          , "back_urls" .= object
              [ "success" .= backUrl "exito"
              , "failure" .= backUrl "error"
              , "pending" .= backUrl "pendiente"
              ]
          , "auto_return"      .= ("approved" :: Text)
          , "notification_url" .= (base <> "/api/webhooks/mercadopago")
          ]
        req = req0 { method = "POST", requestBody = RequestBodyLBS (encode body) }
    resp <- httpLbs req (mpManager cfg)
    pure $ do
      o <- decode (responseBody resp) :: Maybe Object
      prefId    <- textField o "id"
      initPoint <- textField o "init_point"
      pure (prefId, initPoint)

-- | Look up one payment by its id; returns (externalReference, status).
fetchPaymentStatus :: MpConfig -> Text -> IO (Maybe (Text, Text))
fetchPaymentStatus cfg paymentId = case mpAccessToken cfg of
  Nothing -> pure Nothing
  Just token -> do
    req <- authedRequest token
      ("https://api.mercadopago.com/v1/payments/" <> T.unpack paymentId)
    resp <- httpLbs req (mpManager cfg)
    pure $ do
      o      <- decode (responseBody resp) :: Maybe Object
      extRef <- textField o "external_reference"
      status <- textField o "status"
      pure (extRef, status)

-- | Poll for the most recent payment attached to an order reference;
-- returns (paymentId, status).
searchPaymentByRef :: MpConfig -> Text -> IO (Maybe (Text, Text))
searchPaymentByRef cfg ref = case mpAccessToken cfg of
  Nothing -> pure Nothing
  Just token -> do
    req <- authedRequest token
      ("https://api.mercadopago.com/v1/payments/search?sort=date_created&criteria=desc&external_reference="
        <> T.unpack ref)
    resp <- httpLbs req (mpManager cfg)
    pure $ do
      o       <- decode (responseBody resp) :: Maybe Object
      results <- KM.lookup "results" o
      arr     <- case results of Array v -> Just v; _ -> Nothing
      first   <- listToMaybe [ x | Object x <- foldr (:) [] arr ]
      pid     <- case KM.lookup "id" first of
                   Just (Number n) -> Just (T.pack (show (truncate n :: Integer)))
                   Just (String s) -> Just s
                   _               -> Nothing
      status  <- textField first "status"
      pure (pid, status)

textField :: Object -> Key -> Maybe Text
textField o k = case KM.lookup k o of
  Just (String s) -> Just s
  _               -> Nothing

-- | Verify Mercado Pago's x-signature header:
--   x-signature: ts=<ts>,v1=<hmac-sha256-hex>
--   manifest:    "id:<data.id>;request-id:<x-request-id>;ts:<ts>;"
-- Verification is defense-in-depth; the payments-API lookup remains the
-- source of truth. When no secret is configured, everything verifies.
verifySignature :: MpConfig -> Maybe Text -> Maybe Text -> Text -> Bool
verifySignature cfg mSignature mRequestId dataId =
  case mpWebhookSecret cfg of
    Nothing     -> True
    Just secret -> fromMaybe False $ do
      sig <- mSignature
      let parts = [ (T.strip k, T.drop 1 v)
                  | p <- T.splitOn "," sig
                  , let (k, v) = T.breakOn "=" p ]
      ts <- lookup "ts" parts
      v1 <- lookup "v1" parts
      let manifest = "id:" <> T.toLower dataId <> ";"
                   <> maybe "" (\rid -> "request-id:" <> rid <> ";") mRequestId
                   <> "ts:" <> ts <> ";"
          digest = TE.decodeUtf8 . B16.encode $
            SHA256.hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 manifest)
      pure (constantTimeEq (TE.encodeUtf8 digest) (TE.encodeUtf8 (T.toLower v1)))

constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b =
  BS.length a == BS.length b
    && 0 == foldr (\(x, y) acc -> acc + fromEnum (x /= y)) 0 (BS.zip a b)
