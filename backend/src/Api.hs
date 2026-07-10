module Api
  ( API
  , AppEnv(..)
  , app
  ) where

import           Control.Concurrent.MVar    (MVar, withMVar)
import           Control.Exception          (try)
import           Control.Monad              (unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value (..), decodeStrict, object,
                                             (.=))
import qualified Data.Aeson.KeyMap          as KM
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Application)
import           Servant

import           Directo.Types
import qualified Auth
import qualified Db
import qualified MercadoPago                as MP
import qualified OAuth
import qualified Stripe

data AppEnv = AppEnv
  { envConn         :: MVar Connection
  , envMp           :: MP.MpConfig
  , envStripe       :: Stripe.StripeConfig
  , envOAuth        :: OAuth.OAuthEnv
  , envAdminHash    :: ByteString
  , envCookieSecure :: Bool
  , envStore        :: StoreAddress
  , envFallbackWeightGrams :: Int
  }

type CookieHeader = Header "Cookie" Text
type SetCookie a  = Headers '[Header "Set-Cookie" Text] a
type Redirect     = Verb 'GET 302 '[JSON]
                      (Headers '[Header "Location" Text] NoContent)
type RedirectWithCookie = Verb 'GET 302 '[JSON]
                      (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)

type API
  =    "api" :> "health"   :> Get '[JSON] Value
  :<|> "api" :> "products" :> Get '[JSON] [Product]
  :<|> "api" :> "orders"   :> CookieHeader :> ReqBody '[JSON] CheckoutReq :> Post '[JSON] CheckoutResp
  -- social login (provider-generic; /api/auth/config lists what's enabled)
  :<|> "api" :> "auth" :> "config" :> Get '[JSON] AuthConfig
  :<|> "api" :> "auth" :> Capture "provider" Text :> "login" :> Redirect
  :<|> "api" :> "auth" :> Capture "provider" Text :> "callback"
         :> QueryParam "code" Text :> QueryParam "state" Text :> RedirectWithCookie
  :<|> "api" :> "me" :> CookieHeader :> Get '[JSON] UserInfo
  :<|> "api" :> "logout" :> CookieHeader :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "my-orders" :> CookieHeader :> Get '[JSON] [OrderSummary]
  :<|> "api" :> "orders"   :> Capture "ref" Text :> "status" :> Get '[JSON] OrderStatusResp
  :<|> "api" :> "contact"  :> ReqBody '[JSON] ContactReq :> Post '[JSON] NoContent
  :<|> "api" :> "payment-config" :> Get '[JSON] PaymentConfig
  :<|> "api" :> "webhooks" :> "stripe"
         :> Header "Stripe-Signature" Text
         :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent
  :<|> "api" :> "webhooks" :> "mercadopago"
         :> Header "x-signature" Text :> Header "x-request-id" Text
         :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
  :<|> "api" :> "admin" :> "login"  :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "logout" :> CookieHeader :> Post '[JSON] (SetCookie NoContent)
  :<|> "api" :> "admin" :> "me"     :> CookieHeader :> Get '[JSON] NoContent
  :<|> "api" :> "admin" :> "products" :> CookieHeader :> Get '[JSON] [Product]
  :<|> "api" :> "admin" :> "products" :> CookieHeader :> ReqBody '[JSON] ProductInput :> Post '[JSON] Product
  :<|> "api" :> "admin" :> "products" :> Capture "id" Int64 :> CookieHeader :> ReqBody '[JSON] ProductInput :> Put '[JSON] Product
  :<|> "api" :> "admin" :> "products" :> Capture "id" Int64 :> CookieHeader :> Delete '[JSON] NoContent
  :<|> "api" :> "admin" :> "orders" :> QueryParam "status" Text :> CookieHeader :> Get '[JSON] [OrderSummary]
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> CookieHeader :> Get '[JSON] OrderDetail
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> "guia" :> CookieHeader :> Get '[JSON] GuiaDhl
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> "mark-paid" :> CookieHeader :> Post '[JSON] NoContent
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> "ship" :> CookieHeader :> ReqBody '[JSON] ShipReq :> Post '[JSON] NoContent
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> "complete" :> CookieHeader :> Post '[JSON] NoContent
  :<|> "api" :> "admin" :> "orders" :> Capture "id" Int64 :> "cancel"   :> CookieHeader :> Post '[JSON] NoContent
  :<|> "api" :> "admin" :> "customers" :> CookieHeader :> Get '[JSON] [CustomerSummary]
  :<|> "api" :> "admin" :> "customers" :> Capture "email" Text :> "orders" :> CookieHeader :> Get '[JSON] [OrderSummary]
  :<|> "api" :> "admin" :> "messages" :> CookieHeader :> Get '[JSON] [ContactMessage]
  :<|> "api" :> "admin" :> "messages" :> Capture "id" Int64 :> "read" :> CookieHeader :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = serve api (server env)

withDb :: AppEnv -> (Connection -> IO a) -> Handler a
withDb env f = liftIO (withMVar (envConn env) f)

server :: AppEnv -> Server API
server env =
       healthH
  :<|> productsH
  :<|> checkoutH
  :<|> authConfigH
  :<|> authLoginH
  :<|> authCallbackH
  :<|> userMeH
  :<|> userLogoutH
  :<|> myOrdersH
  :<|> orderStatusH
  :<|> contactH
  :<|> paymentConfigH
  :<|> stripeWebhookH
  :<|> webhookH
  :<|> loginH
  :<|> logoutH
  :<|> meH
  :<|> adminProductsH
  :<|> adminProductCreateH
  :<|> adminProductUpdateH
  :<|> adminProductDeleteH
  :<|> adminOrdersH
  :<|> adminOrderDetailH
  :<|> adminGuiaH
  :<|> adminMarkPaidH
  :<|> adminShipH
  :<|> adminCompleteH
  :<|> adminCancelH
  :<|> adminCustomersH
  :<|> adminCustomerOrdersH
  :<|> adminMessagesH
  :<|> adminMessageReadH
  where
    healthH :: Handler Value
    healthH = pure (object ["status" .= ("ok" :: Text)])

    productsH :: Handler [Product]
    productsH = withDb env Db.listProducts

    checkoutH :: Maybe Text -> CheckoutReq -> Handler CheckoutResp
    checkoutH mCookie req = do
      result <- withDb env $ \conn -> try (Db.createOrder conn req)
      case result of
        Left Db.EmptyCart ->
          throwError err400 { errBody = "carrito vacío" }
        Left (Db.UnknownProduct _) ->
          throwError err400 { errBody = "producto desconocido" }
        Left (Db.OutOfStock _) ->
          throwError err409 { errBody = "sin existencias" }
        Right (oid, ref, _total) -> do
          -- Link the order to the signed-in customer, if any.
          mUser <- currentUser env mCookie
          case mUser of
            Just (uid, _) -> void . withDb env $ \conn -> Db.attachOrderUser conn oid uid
            Nothing       -> pure ()
          mDetail <- withDb env (`Db.getOrderDetail` oid)
          let orderLines = maybe [] odLines mDetail
          case paymentProvider req of
            Just "stripe" | Stripe.enabled (envStripe env) -> do
              mSession <- liftIO (Stripe.createCheckoutSession (envStripe env) ref orderLines)
              case mSession of
                Nothing -> pure (CheckoutResp ref Nothing)
                Just (sessionId, url) -> do
                  void . withDb env $ \conn -> do
                    Db.setStripeSession conn oid sessionId
                    Db.transition conn oid PendingPayment (Just "sesión de pago Stripe creada")
                  pure (CheckoutResp ref (Just url))
            _ -> do
              mPref <- liftIO (MP.createPreference (envMp env) ref orderLines)
              case mPref of
                Nothing -> pure (CheckoutResp ref Nothing)
                Just (prefId, initPointUrl) -> do
                  void . withDb env $ \conn -> do
                    Db.setPreference conn oid prefId
                    Db.transition conn oid PendingPayment (Just "preferencia de pago creada")
                  pure (CheckoutResp ref (Just initPointUrl))

    authConfigH :: Handler AuthConfig
    authConfigH = pure (AuthConfig (OAuth.providerNames (envOAuth env)))

    authLoginH :: Text -> Handler (Headers '[Header "Location" Text] NoContent)
    authLoginH providerName =
      case OAuth.lookupProvider (envOAuth env) providerName of
        Nothing -> throwError err404 { errBody = "proveedor no habilitado" }
        Just p  -> do
          url <- liftIO (OAuth.authorizeUrl (envOAuth env) p)
          pure (addHeader url NoContent)

    authCallbackH :: Text -> Maybe Text -> Maybe Text
                  -> Handler (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
    authCallbackH providerName mCode mState = do
      let failRedirect = pure $
            addHeader "/#/cuenta?login=error" (addHeader "" NoContent)
      case (OAuth.lookupProvider (envOAuth env) providerName, mCode, mState) of
        (Just p, Just code, Just st) -> do
          okState <- liftIO (OAuth.consumeState (envOAuth env) st)
          if not okState then failRedirect else do
            mClaims <- liftIO (OAuth.exchangeCode (envOAuth env) p code)
            case mClaims of
              Nothing -> failRedirect
              Just claims -> do
                token <- liftIO Auth.generateToken
                void . withDb env $ \conn -> do
                  uid <- Db.upsertUser conn providerName
                           (OAuth.cSubject claims) (OAuth.cEmail claims)
                           (OAuth.cName claims) (OAuth.cPicture claims)
                  Db.insertUserSession conn token uid
                pure $ addHeader "/#/cuenta?login=ok"
                  (addHeader (Auth.userCookie (envCookieSecure env) token) NoContent)
        _ -> failRedirect

    userMeH :: Maybe Text -> Handler UserInfo
    userMeH mCookie = do
      mUser <- currentUser env mCookie
      maybe (throwError err401) (pure . snd) mUser

    userLogoutH :: Maybe Text -> Handler (SetCookie NoContent)
    userLogoutH mCookie = do
      case mCookie >>= Auth.userCookieToken of
        Just token -> void $ withDb env (`Db.deleteUserSession` token)
        Nothing    -> pure ()
      pure (addHeader (Auth.clearUserCookie (envCookieSecure env)) NoContent)

    myOrdersH :: Maybe Text -> Handler [OrderSummary]
    myOrdersH mCookie = do
      mUser <- currentUser env mCookie
      case mUser of
        Nothing       -> throwError err401
        Just (uid, _) -> withDb env $ \conn -> Db.listOrdersForUser conn uid

    orderStatusH :: Text -> Handler OrderStatusResp
    orderStatusH ref = do
      mStatus <- withDb env (`Db.getOrderStatusByRef` ref)
      case mStatus of
        Nothing -> throwError err404
        Just st -> do
          -- Poll Mercado Pago (rate-limited) while payment is pending, so
          -- the flow works even without a reachable webhook.
          st' <- if st == PendingPayment
            then do
              mOid <- withDb env (`Db.getOrderIdByRef` ref)
              case mOid of
                Nothing  -> pure st
                Just oid -> do
                  due <- withDb env $ \conn -> Db.mpCheckDue conn oid 30
                  if not due then pure st else do
                    mInfo <- withDb env (`Db.paymentInfo` oid)
                    case mInfo of
                      Just (Just "stripe", Just sessionId) -> do
                        mPayStatus <- liftIO
                          (Stripe.fetchSessionStatus (envStripe env) sessionId)
                        case mPayStatus of
                          Just "paid" -> void . withDb env $ \conn ->
                            Db.transition conn oid Paid
                              (Just ("pago Stripe " <> sessionId))
                          _ -> pure ()
                      _ -> do
                        mPayment <- liftIO (MP.searchPaymentByRef (envMp env) ref)
                        case mPayment of
                          Nothing -> pure ()
                          Just (pid, mpStatus) ->
                            applyPaymentStatus env oid pid mpStatus
                    newStatus <- withDb env (`Db.getOrderStatusByRef` ref)
                    pure (fromMaybe st newStatus)
            else pure st
          pure (OrderStatusResp ref st')

    contactH :: ContactReq -> Handler NoContent
    contactH cr = do
      let bad = T.null . T.strip
      when (bad (crName cr) || bad (crEmail cr) || bad (crMessage cr)) $
        throwError err400 { errBody = "nombre, email y mensaje son obligatorios" }
      withDb env (`Db.insertContactMessage` cr)
      pure NoContent

    paymentConfigH :: Handler PaymentConfig
    paymentConfigH = pure . PaymentConfig $ concat
      [ [ "mercadopago" | isJust (MP.mpAccessToken (envMp env)) ]
      , [ "stripe"      | Stripe.enabled (envStripe env) ]
      ]

    -- Stripe signs the raw body, so this route takes bytes and decodes
    -- JSON only after verification.
    stripeWebhookH :: Maybe Text -> ByteString -> Handler NoContent
    stripeWebhookH mSig rawBody = do
      when (Stripe.verifySignature (envStripe env) mSig rawBody) $
        case decodeStrict rawBody :: Maybe Value of
          Just (Object o)
            | Just (String evType) <- KM.lookup "type" o
            , Just (Object dat)    <- KM.lookup "data" o
            , Just (Object obj)    <- KM.lookup "object" dat
            , Just (String sid)    <- KM.lookup "id" obj -> do
                mOid <- withDb env (`Db.getOrderIdByStripeSession` sid)
                case mOid of
                  Nothing  -> pure ()
                  Just oid -> case evType of
                    t | t `elem` [ "checkout.session.completed"
                                 , "checkout.session.async_payment_succeeded" ]
                      , Just (String "paid") <- KM.lookup "payment_status" obj ->
                        void . withDb env $ \conn ->
                          Db.transition conn oid Paid (Just ("pago Stripe " <> sid))
                    t | t `elem` [ "checkout.session.async_payment_failed"
                                 , "checkout.session.expired" ] ->
                        void . withDb env $ \conn ->
                          Db.transition conn oid Cancelled (Just ("Stripe " <> t))
                    _ -> pure ()
          _ -> pure ()
      -- Always 200: Stripe retries on anything else.
      pure NoContent

    webhookH :: Maybe Text -> Maybe Text -> Value -> Handler NoContent
    webhookH mSig mReqId payload = do
      let mDataId = case payload of
            Object o -> case KM.lookup "data" o of
              Just (Object d) -> case KM.lookup "id" d of
                Just (String s) -> Just s
                Just (Number n) -> Just (T.pack (show (truncate n :: Integer)))
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing
          isPayment = case payload of
            Object o -> case KM.lookup "type" o of
              Just (String "payment") -> True
              _ -> case KM.lookup "topic" o of
                Just (String "payment") -> True
                _ -> False
            _ -> False
      case mDataId of
        Just dataId | isPayment -> do
          when (MP.verifySignature (envMp env) mSig mReqId dataId) $ do
            mPayment <- liftIO (MP.fetchPaymentStatus (envMp env) dataId)
            case mPayment of
              Nothing -> pure ()
              Just (extRef, mpStatus) -> do
                mOid <- withDb env (`Db.getOrderIdByRef` extRef)
                case mOid of
                  Nothing  -> pure ()
                  Just oid -> applyPaymentStatus env oid dataId mpStatus
        _ -> pure ()
      -- Always 200: Mercado Pago retries on anything else.
      pure NoContent

    loginH :: LoginRequest -> Handler (SetCookie NoContent)
    loginH (LoginRequest password) =
      if Auth.verifyPassword (envAdminHash env) password
        then do
          token <- liftIO Auth.generateToken
          void $ withDb env (`Db.insertSession` token)
          pure (addHeader (Auth.sessionCookie (envCookieSecure env) token) NoContent)
        else throwError err401 { errBody = "contraseña incorrecta" }

    logoutH :: Maybe Text -> Handler (SetCookie NoContent)
    logoutH mCookie = do
      case mCookie >>= Auth.cookieToken of
        Just token -> void $ withDb env (`Db.deleteSession` token)
        Nothing    -> pure ()
      pure (addHeader (Auth.clearSessionCookie (envCookieSecure env)) NoContent)

    meH :: Maybe Text -> Handler NoContent
    meH mCookie = requireAdmin env mCookie >> pure NoContent

    adminProductsH mCookie =
      requireAdmin env mCookie >> withDb env Db.listAllProducts

    adminProductCreateH mCookie input = do
      requireAdmin env mCookie
      withDb env (`Db.insertProduct` input)

    adminProductUpdateH pid mCookie input = do
      requireAdmin env mCookie
      mProduct <- withDb env $ \conn -> Db.updateProduct conn pid input
      maybe (throwError err404) pure mProduct

    adminProductDeleteH pid mCookie = do
      requireAdmin env mCookie
      withDb env (`Db.deactivateProduct` pid)
      pure NoContent

    adminOrdersH mStatusText mCookie = do
      requireAdmin env mCookie
      let mStatus = mStatusText >>= orderStatusFromText
      withDb env $ \conn -> Db.listOrders conn mStatus

    adminOrderDetailH oid mCookie = do
      requireAdmin env mCookie
      mDetail <- withDb env (`Db.getOrderDetail` oid)
      maybe (throwError err404) pure mDetail

    adminGuiaH oid mCookie = do
      requireAdmin env mCookie
      mGuia <- withDb env $ \conn ->
        Db.guiaFor conn (envStore env) (envFallbackWeightGrams env) oid
      maybe (throwError err404) pure mGuia

    adminMarkPaidH oid mCookie =
      adminTransition oid mCookie Paid (Just "pago confirmado manualmente")

    adminShipH oid mCookie (ShipReq waybill) = do
      requireAdmin env mCookie
      ok <- withDb env $ \conn -> do
        moved <- Db.transition conn oid Shipped (Just ("guía DHL: " <> waybill))
        when moved (Db.setWaybill conn oid waybill)
        pure moved
      unless ok (throwError err409 { errBody = "transición inválida" })
      pure NoContent

    adminCompleteH oid mCookie =
      adminTransition oid mCookie Completed Nothing

    adminCancelH oid mCookie =
      adminTransition oid mCookie Cancelled (Just "cancelado por administrador")

    adminCustomersH mCookie =
      requireAdmin env mCookie >> withDb env Db.listCustomers

    adminCustomerOrdersH email mCookie = do
      requireAdmin env mCookie
      withDb env $ \conn -> Db.listOrdersForEmail conn email

    adminMessagesH mCookie =
      requireAdmin env mCookie >> withDb env Db.listContactMessages

    adminMessageReadH mid mCookie = do
      requireAdmin env mCookie
      withDb env $ \conn -> Db.markMessageRead conn mid
      pure NoContent

    adminTransition oid mCookie to note = do
      requireAdmin env mCookie
      ok <- withDb env $ \conn -> Db.transition conn oid to note
      unless ok (throwError err409 { errBody = "transición inválida" })
      pure NoContent

-- | Map a Mercado Pago payment status onto the order state machine.
applyPaymentStatus :: AppEnv -> Int64 -> Text -> Text -> Handler ()
applyPaymentStatus env oid paymentId mpStatus = do
  void . withDb env $ \conn -> do
    Db.recordPayment conn oid paymentId mpStatus
    case mpStatus of
      "approved" -> Db.transition conn oid Paid (Just ("pago MP " <> paymentId))
      s | s `elem` ["rejected", "cancelled", "refunded", "charged_back"] ->
        Db.transition conn oid Cancelled (Just ("pago MP " <> s))
      _ -> pure False

currentUser :: AppEnv -> Maybe Text -> Handler (Maybe (Int64, UserInfo))
currentUser env mCookie =
  case mCookie >>= Auth.userCookieToken of
    Nothing    -> pure Nothing
    Just token -> withDb env (`Db.userForSession` token)

requireAdmin :: AppEnv -> Maybe Text -> Handler ()
requireAdmin env mCookie =
  case mCookie >>= Auth.cookieToken of
    Nothing -> throwError err401
    Just token -> do
      ok <- withDb env (`Db.sessionExists` token)
      unless ok (throwError err401)
