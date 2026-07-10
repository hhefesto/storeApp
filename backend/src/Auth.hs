module Auth
  ( getPasswordHash
  , verifyPassword
  , generateToken
  , sessionCookie
  , clearSessionCookie
  , cookieToken
  ) where

import           Crypto.BCrypt         (validatePassword)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC8
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.UUID             (toText)
import           Data.UUID.V4          (nextRandom)
import           System.Environment    (lookupEnv)
import           System.IO             (hPutStrLn, stderr)

getPasswordHash :: IO ByteString
getPasswordHash = do
  mFile <- lookupEnv "DIRECTO_ADMIN_PASSWORD_HASH_FILE"
  mEnv  <- lookupEnv "DIRECTO_ADMIN_PASSWORD_HASH"
  case (mFile, mEnv) of
    (Just path, _) -> BC8.pack <$> readFile path
    (_, Just hash) | not (null hash) -> pure (BC8.pack hash)
    _ -> do
      hPutStrLn stderr "DIRECTO_ADMIN_PASSWORD_HASH is not set; using development admin password \"directo\"."
      pure devPasswordHash

-- bcrypt of "directo" — development only; production loads the agenix hash.
devPasswordHash :: ByteString
devPasswordHash =
  "$2y$12$8Fw/.JkkuhQNEN7xrWb2nuPfI2cher80HNMRmfpiYNasbFQppl1aK"

verifyPassword :: ByteString -> Text -> Bool
verifyPassword hash password =
  validatePassword (BC8.takeWhile (/= '\n') hash) (BC8.pack (T.unpack password))

generateToken :: IO Text
generateToken = toText <$> nextRandom

sessionCookie :: Bool -> Text -> Text
sessionCookie secure token =
  "directo_admin=" <> token
    <> "; Path=/; HttpOnly; SameSite=Strict; Max-Age=86400"
    <> (if secure then "; Secure" else "")

clearSessionCookie :: Bool -> Text
clearSessionCookie secure =
  "directo_admin=deleted; Path=/; HttpOnly; SameSite=Strict; Max-Age=0"
    <> (if secure then "; Secure" else "")

-- | Extract the admin session token from a Cookie header value.
cookieToken :: Text -> Maybe Text
cookieToken header =
  let pairs = map (T.breakOn "=") (map T.strip (T.splitOn ";" header))
  in T.drop 1 . snd <$> lookup' "directo_admin" pairs
  where
    lookup' k = foldr (\(k', v) acc -> if k' == k then Just (k', v) else acc) Nothing
