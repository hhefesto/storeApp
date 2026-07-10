module OrderRef
  ( newOrderRef
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (getCurrentTime, formatTime,
                                             defaultTimeLocale)
import           Database.PostgreSQL.Simple (Connection, Only (..), query_)

-- | Generate a reference like DIR-20260709-0042. The numeric part comes
-- from a global sequence so references never collide; the date prefix is
-- informational (what the admin reads to DHL).
newOrderRef :: Connection -> IO Text
newOrderRef conn = do
  now <- getCurrentTime
  [Only (n :: Int)] <- query_ conn "SELECT nextval('order_ref_seq')::int"
  let day = formatTime defaultTimeLocale "%Y%m%d" now
      num = T.justifyRight 4 '0' (T.pack (show n))
  pure $ "DIR-" <> T.pack day <> "-" <> num
