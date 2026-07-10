module Main where

import           Control.Concurrent.MVar  (newMVar)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)
import           System.IO                (BufferMode (..), hSetBuffering, stdout)

import qualified Api
import qualified Auth
import qualified Db
import           Directo.Types            (StoreAddress (..))
import qualified MercadoPago              as MP
import qualified OAuth

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port      <- maybe 3002 read <$> lookupEnv "DIRECTO_PORT"
  conn      <- Db.initDb
  connVar   <- newMVar conn
  mp        <- MP.loadConfig
  oauth     <- OAuth.loadOAuthEnv (MP.mpPublicBaseUrl mp) (MP.mpManager mp)
  adminHash <- Auth.getPasswordHash
  secure    <- (== Just "1") <$> lookupEnv "DIRECTO_COOKIE_SECURE"
  store     <- loadStoreAddress
  fallbackW <- maybe 500 read <$> lookupEnv "DIRECTO_FALLBACK_WEIGHT_GRAMS"
  putStrLn $ "directo-backend listening on port " <> show port
  Warp.run port $ Api.app Api.AppEnv
    { Api.envConn                = connVar
    , Api.envMp                  = mp
    , Api.envOAuth               = oauth
    , Api.envAdminHash           = adminHash
    , Api.envCookieSecure        = secure
    , Api.envStore               = store
    , Api.envFallbackWeightGrams = fallbackW
    }

loadStoreAddress :: IO StoreAddress
loadStoreAddress = do
  storeName  <- fromMaybe "Directo — Refacciones para Electrodomésticos"
                  <$> lookupEnv "DIRECTO_STORE_NAME"
  storeAddr  <- fromMaybe "Querétaro, Qro., México"
                  <$> lookupEnv "DIRECTO_STORE_ADDRESS"
  storePhone <- fromMaybe "442-215-4990" <$> lookupEnv "DIRECTO_STORE_PHONE"
  pure StoreAddress
    { saName         = T.pack storeName
    , saAddressLines = T.splitOn "|" (T.pack storeAddr)
    , saPhone        = T.pack storePhone
    }
