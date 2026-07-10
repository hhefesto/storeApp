-- | Provider-generic OAuth2 / OpenID Connect login (authorization-code flow).
--
-- Google ships enabled out of the box; adding another provider is one more
-- 'Provider' record wired from env vars in 'loadProviders'. Credentials come
-- from the environment:
--
--   DIRECTO_GOOGLE_CLIENT_ID / DIRECTO_GOOGLE_CLIENT_SECRET
--
-- A provider with no credentials simply doesn't appear in /api/auth/config,
-- so development without secrets shows no social-login buttons.
module OAuth
  ( Provider(..)
  , OAuthEnv(..)
  , loadOAuthEnv
  , providerNames
  , lookupProvider
  , authorizeUrl
  , newState
  , consumeState
  , exchangeCode
  , Claims(..)
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar)
import           Data.Aeson              (Object, Value (..), decode)
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy    as BL
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import           Data.Time               (UTCTime, addUTCTime, getCurrentTime)
import           Data.UUID               (toText)
import           Data.UUID.V4            (nextRandom)
import           Network.HTTP.Client
import           Network.HTTP.Types      (renderSimpleQuery)
import           System.Environment      (lookupEnv)

data Provider = Provider
  { pName         :: Text  -- ^ URL slug, e.g. "google"
  , pAuthEndpoint :: Text
  , pTokenEndpoint:: Text
  , pScope        :: Text
  , pClientId     :: Text
  , pClientSecret :: Text
  }

data OAuthEnv = OAuthEnv
  { oaProviders :: [Provider]
  , oaBaseUrl   :: Text                        -- ^ public base URL
  , oaStates    :: MVar (Map Text UTCTime)     -- ^ pending anti-CSRF states
  , oaManager   :: Manager
  }

loadOAuthEnv :: Text -> Manager -> IO OAuthEnv
loadOAuthEnv baseUrl manager = do
  google <- do
    cid <- lookupEnv "DIRECTO_GOOGLE_CLIENT_ID"
    sec <- lookupEnv "DIRECTO_GOOGLE_CLIENT_SECRET"
    pure $ case (cid, sec) of
      (Just i, Just s) | not (null i) && not (null s) -> Just Provider
        { pName          = "google"
        , pAuthEndpoint  = "https://accounts.google.com/o/oauth2/v2/auth"
        , pTokenEndpoint = "https://oauth2.googleapis.com/token"
        , pScope         = "openid email profile"
        , pClientId      = T.pack i
        , pClientSecret  = T.pack s
        }
      _ -> Nothing
  states <- newMVar M.empty
  pure OAuthEnv
    { oaProviders = [ p | Just p <- [google] ]
    , oaBaseUrl   = baseUrl
    , oaStates    = states
    , oaManager   = manager
    }

providerNames :: OAuthEnv -> [Text]
providerNames = map pName . oaProviders

lookupProvider :: OAuthEnv -> Text -> Maybe Provider
lookupProvider env n =
  case [ p | p <- oaProviders env, pName p == n ] of
    (p:_) -> Just p
    []    -> Nothing

redirectUri :: OAuthEnv -> Provider -> Text
redirectUri env p = oaBaseUrl env <> "/api/auth/" <> pName p <> "/callback"

-- | Mint an anti-CSRF state (valid 10 minutes) and build the authorize URL.
authorizeUrl :: OAuthEnv -> Provider -> IO Text
authorizeUrl env p = do
  st <- newState env
  let q = renderSimpleQuery True
        [ ("client_id",     TE.encodeUtf8 (pClientId p))
        , ("redirect_uri",  TE.encodeUtf8 (redirectUri env p))
        , ("response_type", "code")
        , ("scope",         TE.encodeUtf8 (pScope p))
        , ("state",         TE.encodeUtf8 st)
        ]
  pure (pAuthEndpoint p <> TE.decodeUtf8 q)

newState :: OAuthEnv -> IO Text
newState env = do
  st  <- toText <$> nextRandom
  now <- getCurrentTime
  modifyMVar_ (oaStates env) (pure . M.insert st (addUTCTime 600 now))
  pure st

-- | Validate and burn a state token.
consumeState :: OAuthEnv -> Text -> IO Bool
consumeState env st = do
  now <- getCurrentTime
  modifyMVar (oaStates env) $ \m ->
    let live = M.filter (> now) m
    in pure (M.delete st live, M.member st live)

-- | OIDC claims we keep from the provider's id_token.
data Claims = Claims
  { cSubject :: Text
  , cEmail   :: Text
  , cName    :: Text
  , cPicture :: Maybe Text
  } deriving Show

-- | Exchange the authorization code for tokens and read the id_token
-- claims. The token arrives over TLS directly from the provider's token
-- endpoint, so the JWT payload is trusted without signature verification
-- (the standard shortcut for confidential-client code flows).
exchangeCode :: OAuthEnv -> Provider -> Text -> IO (Maybe Claims)
exchangeCode env p code = do
  req0 <- parseRequest (T.unpack (pTokenEndpoint p))
  let req = urlEncodedBody
        [ ("code",          TE.encodeUtf8 code)
        , ("client_id",     TE.encodeUtf8 (pClientId p))
        , ("client_secret", TE.encodeUtf8 (pClientSecret p))
        , ("redirect_uri",  TE.encodeUtf8 (redirectUri env p))
        , ("grant_type",    "authorization_code")
        ] req0
  resp <- httpLbs req (oaManager env)
  pure $ do
    o       <- decode (responseBody resp) :: Maybe Object
    idToken <- case KM.lookup "id_token" o of
                 Just (String s) -> Just s
                 _               -> Nothing
    claims  <- jwtPayload idToken
    subj    <- textField claims "sub"
    email   <- textField claims "email"
    pure Claims
      { cSubject = subj
      , cEmail   = email
      , cName    = fromMaybe email (textField claims "name")
      , cPicture = textField claims "picture"
      }

-- | Decode the (unverified) payload segment of a JWT.
jwtPayload :: Text -> Maybe Object
jwtPayload jwt = case T.splitOn "." jwt of
  (_:payload:_) ->
    case B64URL.decodeUnpadded (TE.encodeUtf8 payload) of
      Right bs -> decode (BL.fromStrict bs)
      Left _   -> case B64URL.decode (TE.encodeUtf8 (pad payload)) of
        Right bs -> decode (BL.fromStrict bs)
        Left _   -> Nothing
  _ -> Nothing
  where
    pad t = t <> T.replicate ((4 - T.length t `mod` 4) `mod` 4) "="

textField :: Object -> KM.Key -> Maybe Text
textField o k = case KM.lookup k o of
  Just (String s) -> Just s
  _               -> Nothing
