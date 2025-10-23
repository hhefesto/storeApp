{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Int        (Int32, Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Foldable   (for_)
import           GHC.Generics    (Generic)

-- hasql (Rel8 uses hasql underneath)
import qualified Hasql.Connection as H
import qualified Hasql.Session    as H

-- Rel8
import           Rel8
-- Aeson already has instances for Map Int Int
import           Data.Aeson      (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Domain types (as you provided)
--------------------------------------------------------------------------------

type PID = Int
type SID = Int

data Product = Product
  { productId       :: PID
  , productName     :: Text
  , productCategory :: Text
  , productPrice    :: Double
  , productIcon     :: Text
  } deriving (Eq, Show, Generic)

data ShipmentStatus = Unsent | Sent | Completed
  deriving (Eq, Show, Generic)

data Shipment = Shipment
  { shItems  :: Map PID Int      -- product -> quantity
  , shStatus :: ShipmentStatus
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- DB encodings
-- - ShipmentStatus as TEXT (manual encode/decode)
-- - shItems as JSONB via DerivingVia (Aeson Map instances already exist)
--------------------------------------------------------------------------------

encodeStatus :: ShipmentStatus -> Text
encodeStatus Unsent    = "unsent"
encodeStatus Sent      = "sent"
encodeStatus Completed = "completed"

decodeStatus :: Text -> Maybe ShipmentStatus
decodeStatus "unsent"    = Just Unsent
decodeStatus "sent"      = Just Sent
decodeStatus "completed" = Just Completed
decodeStatus _           = Nothing

-- Make (Map Int Int) a jsonb column type via Rel8's JSONBEncoded wrapper.
-- This relies on Aeson's existing instances; no standalone deriving needed.
deriving via (JSONBEncoded (Map Int Int)) instance DBType (Map Int Int)

--------------------------------------------------------------------------------
-- Rel8 table types & schemas (HKD)
--------------------------------------------------------------------------------

data ProductT f = ProductT
  { pId       :: Column f Int32
  , pName     :: Column f Text
  , pCategory :: Column f Text
  , pPrice    :: Column f Double
  , pIcon     :: Column f Text
  } deriving stock Generic
    deriving anyclass Rel8able

productSchema :: TableSchema (ProductT Name)
productSchema =
  TableSchema
    { name = "products"
    , schema = Nothing
    , columns = ProductT
        { pId       = "id"
        , pName     = "name"
        , pCategory = "category"
        , pPrice    = "price"
        , pIcon     = "icon"
        }
    }

data ShipmentT f = ShipmentT
  { sId     :: Column f Int32
  , sItems  :: Column f (Map Int Int)  -- stored as jsonb via the DBType instance
  , sStatus :: Column f Text           -- encoded ShipmentStatus
  } deriving stock Generic
    deriving anyclass Rel8able

shipmentSchema :: TableSchema (ShipmentT Name)
shipmentSchema =
  TableSchema
    { name = "shipments"
    , schema = Nothing
    , columns = ShipmentT
        { sId     = "sid"
        , sItems  = "items"
        , sStatus = "status"
        }
    }

--------------------------------------------------------------------------------
-- Domain <-> Row conversions
--------------------------------------------------------------------------------

toDomainProduct :: ProductT Result -> Product
toDomainProduct (ProductT pid nm cat pr ic) =
  Product
    { productId       = fromIntegral pid
    , productName     = nm
    , productCategory = cat
    , productPrice    = pr
    , productIcon     = ic
    }

fromDomainProduct :: Product -> ProductT Expr
fromDomainProduct Product{..} =
  ProductT
    { pId       = lit (fromIntegral @Int @Int32 productId)
    , pName     = lit productName
    , pCategory = lit productCategory
    , pPrice    = lit productPrice
    , pIcon     = lit productIcon
    }

toDomainShipment :: ShipmentT Result -> (SID, Shipment)
toDomainShipment (ShipmentT sid items stTxt) =
  let st = maybe (error $ "Unknown status in DB: " <> T.unpack stTxt)
                 id
                 (decodeStatus stTxt)
  in (fromIntegral sid, Shipment { shItems = items, shStatus = st })

fromDomainShipment :: SID -> Shipment -> ShipmentT Expr
fromDomainShipment sid Shipment{..} =
  ShipmentT
    { sId     = lit (fromIntegral @Int @Int32 sid)
    , sItems  = lit shItems
    , sStatus = lit (encodeStatus shStatus)
    }

--------------------------------------------------------------------------------
-- DDL
--------------------------------------------------------------------------------

ddl :: H.Session ()
ddl = do
  H.sql
    "CREATE TABLE IF NOT EXISTS products (\
    \  id        INT PRIMARY KEY,\
    \  name      TEXT NOT NULL,\
    \  category  TEXT NOT NULL,\
    \  price     DOUBLE PRECISION NOT NULL,\
    \  icon      TEXT NOT NULL\
    \);"

  H.sql
    "CREATE TABLE IF NOT EXISTS shipments (\
    \  sid     INT PRIMARY KEY,\
    \  items   JSONB NOT NULL,\
    \  status  TEXT NOT NULL CHECK (status IN ('unsent','sent','completed'))\
    \);"

--------------------------------------------------------------------------------
-- Demo data
--------------------------------------------------------------------------------

demoProducts :: [Product]
demoProducts =
  [ Product 1 "Tennis Racket"  "Sports"    129.99 "tennis"
  , Product 2 "Running Shoes"  "Footwear"   89.50 "shoes"
  , Product 3 "Coffee Grinder" "Appliance"  54.00 "coffee"
  ]

demoShipment :: (SID, Shipment)
demoShipment =
  ( 1001
  , Shipment { shItems = M.fromList [(1,2),(3,1)], shStatus = Unsent }
  )

--------------------------------------------------------------------------------
-- Rel8 actions (wrap Statements with H.statement () to run as Sessions)
--------------------------------------------------------------------------------

insertProducts :: [Product] -> H.Session Int64
insertProducts ps =
  H.statement () $
    insert $ Insert
      { into        = productSchema
      , rows        = values (fromDomainProduct <$> ps)
      , onConflict  = DoNothing
      , returning   = NumberOfRowsAffected
      }

insertShipment :: SID -> Shipment -> H.Session Int64
insertShipment sid sh =
  H.statement () $
    insert $ Insert
      { into        = shipmentSchema
      , rows        = values [fromDomainShipment sid sh]
      , onConflict  = DoNothing
      , returning   = NumberOfRowsAffected
      }

selectAllProducts :: H.Session [Product]
selectAllProducts = do
  rows <- H.statement () $ select (each productSchema)
  pure (toDomainProduct <$> rows)

selectAllShipments :: H.Session [(SID, Shipment)]
selectAllShipments = do
  rows <- H.statement () $ select (each shipmentSchema)
  pure (toDomainShipment <$> rows)

setShipmentStatus :: SID -> ShipmentStatus -> H.Session Int64
setShipmentStatus sid newSt =
  H.statement () $
    update $ Update
      { target      = shipmentSchema
      , from        = pure ()  -- Query ()
      , set         = \_ row -> row { sStatus = lit (encodeStatus newSt) }
      , updateWhere = \_ row -> sId row ==. lit (fromIntegral @Int @Int32 sid)
      , returning   = NumberOfRowsAffected
      }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let connStr = "postgres://postgres:postgres@localhost:5432/webstore"

  putStrLn "Connecting to PostgreSQL…"
  econn <- H.acquire connStr
  conn  <- case econn of
    Left err -> error ("Could not connect: " <> show err)
    Right c  -> pure c

  putStrLn "Ensuring tables exist…"
  runOrDie conn ddl

  putStrLn "Seeding demo products…"
  _ <- runOrDie conn (insertProducts demoProducts)

  let (sid0, sh0) = demoShipment
  putStrLn $ "Creating shipment SID=" <> show sid0
  _ <- runOrDie conn (insertShipment sid0 sh0)

  putStrLn "\nProducts:"
  prods <- runOrDie conn selectAllProducts
  for_ prods print

  putStrLn "\nShipments (before):"
  shs1 <- runOrDie conn selectAllShipments
  for_ shs1 print

  putStrLn ("\nMarking SID=" <> show sid0 <> " as Sent…")
  _ <- runOrDie conn (setShipmentStatus sid0 Sent)

  putStrLn "\nShipments (after):"
  shs2 <- runOrDie conn selectAllShipments
  for_ shs2 print

  putStrLn "\nDone."

-- Small helper: run a Session or crash with error (good enough for a demo)
runOrDie :: H.Connection -> H.Session a -> IO a
runOrDie conn sess = do
  er <- H.run sess conn
  case er of
    Left e  -> error (show e)
    Right x -> pure x
