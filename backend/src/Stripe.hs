-- | Minimal Stripe Checkout (hosted page) client.
--
-- Stripe is the wallet provider: Apple Pay and Google Pay surface
-- automatically on Stripe's hosted checkout page for eligible
-- devices/browsers once enabled in the Stripe Dashboard — no extra
-- integration or Apple domain verification needed on our side.
--
-- Configured entirely through the environment:
--   STRIPE_SECRET_KEY       — enables Stripe when present
--   STRIPE_WEBHOOK_SECRET   — enables Stripe-Signature verification
--   DIRECTO_PUBLIC_BASE_URL — success_url / cancel_url base
module Stripe
  ( StripeConfig(..)
  , loadConfig
  , enabled
  , createCheckoutSession
  , fetchSessionStatus
  , verifySignature
  ) where

import qualified Crypto.Hash.SHA256      as SHA256
import           Data.Aeson
import           Data.Aeson.Key          (Key)
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as B16
import           Data.Maybe              (fromMaybe, isJust)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (newTlsManager)
import           System.Environment      (lookupEnv)

import           Directo.Types

data StripeConfig = StripeConfig
  { stSecretKey     :: Maybe Text
  , stWebhookSecret :: Maybe Text
  , stPublicBaseUrl :: Text
  , stManager       :: Manager
  }

loadConfig :: IO StripeConfig
loadConfig = do
  key     <- fmap T.pack <$> lookupEnv "STRIPE_SECRET_KEY"
  secret  <- fmap T.pack <$> lookupEnv "STRIPE_WEBHOOK_SECRET"
  baseUrl <- fromMaybe "http://localhost" . fmap T.pack
               <$> lookupEnv "DIRECTO_PUBLIC_BASE_URL"
  manager <- newTlsManager
  pure (StripeConfig key secret baseUrl manager)

enabled :: StripeConfig -> Bool
enabled = isJust . stSecretKey

-- | Create a hosted Checkout Session; returns (sessionId, url).
-- Stripe's API is form-encoded; amounts are MXN centavos, which is
-- exactly our unit.
createCheckoutSession :: StripeConfig -> Text -> [OrderLine] -> IO (Maybe (Text, Text))
createCheckoutSession cfg ref orderLines = case stSecretKey cfg of
  Nothing -> pure Nothing
  Just key -> do
    req0 <- parseRequest "https://api.stripe.com/v1/checkout/sessions"
    let base = stPublicBaseUrl cfg
        params =
          [ ("mode", "payment")
          , ("client_reference_id", TE.encodeUtf8 ref)
          , ("success_url", TE.encodeUtf8 (base <> "/#/pago/exito?ref=" <> ref))
          , ("cancel_url",  TE.encodeUtf8 (base <> "/#/pago/error?ref=" <> ref))
          ] ++ concat
          [ [ (li "price_data][currency",              "mxn")
            , (li "price_data][product_data][name",    TE.encodeUtf8 (olProductName l))
            , (li "price_data][unit_amount",           TE.encodeUtf8 (tshow (olUnitPriceCents l)))
            , (li "quantity",                          TE.encodeUtf8 (tshow (olQuantity l)))
            ]
          | (i, l) <- zip [0 :: Int ..] orderLines
          , let li field = "line_items[" <> TE.encodeUtf8 (tshow i) <> "][" <> field <> "]"
          ]
        reqForm = urlEncodedBody params req0
        req = reqForm
          { requestHeaders =
              ("Authorization", "Bearer " <> TE.encodeUtf8 key)
              : requestHeaders reqForm
          }
    resp <- httpLbs req (stManager cfg)
    pure $ do
      o   <- decode (responseBody resp) :: Maybe Object
      sid <- textField o "id"
      url <- textField o "url"
      pure (sid, url)
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

-- | Look up a Checkout Session; returns its payment_status
-- ("paid" | "unpaid" | "no_payment_required") — the polling fallback
-- when the webhook can't reach us.
fetchSessionStatus :: StripeConfig -> Text -> IO (Maybe Text)
fetchSessionStatus cfg sessionId = case stSecretKey cfg of
  Nothing -> pure Nothing
  Just key -> do
    req0 <- parseRequest
      ("https://api.stripe.com/v1/checkout/sessions/" <> T.unpack sessionId)
    let req = req0 { requestHeaders =
                       [("Authorization", "Bearer " <> TE.encodeUtf8 key)] }
    resp <- httpLbs req (stManager cfg)
    pure $ do
      o <- decode (responseBody resp) :: Maybe Object
      textField o "payment_status"

textField :: Object -> Key -> Maybe Text
textField o k = case KM.lookup k o of
  Just (String s) -> Just s
  _               -> Nothing

-- | Verify Stripe's Stripe-Signature header over the raw request body:
--   Stripe-Signature: t=<ts>,v1=<hmac-sha256-hex>[,v1=…]
--   signed payload:   "<ts>.<raw body>"
-- When no secret is configured, everything verifies (the session-API
-- lookup remains the source of truth).
verifySignature :: StripeConfig -> Maybe Text -> BS.ByteString -> Bool
verifySignature cfg mSignature rawBody =
  case stWebhookSecret cfg of
    Nothing     -> True
    Just secret -> fromMaybe False $ do
      sig <- mSignature
      let parts = [ (T.strip k, T.drop 1 v)
                  | p <- T.splitOn "," sig
                  , let (k, v) = T.breakOn "=" p ]
      ts <- lookup "t" parts
      let v1s = [ v | ("v1", v) <- parts ]
          payload = TE.encodeUtf8 ts <> "." <> rawBody
          digest = B16.encode (SHA256.hmac (TE.encodeUtf8 secret) payload)
      pure (any (constantTimeEq digest . TE.encodeUtf8 . T.toLower) v1s)

constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b =
  BS.length a == BS.length b
    && 0 == foldr (\(x, y) acc -> acc + fromEnum (x /= y)) 0 (BS.zip a b)
