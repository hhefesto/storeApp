module Main where

import qualified Data.ByteString.Char8                as BC8
import           Database.PostgreSQL.Simple           (close, connectPostgreSQL,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       runMigration)
import           System.Environment                   (lookupEnv)
import           System.Exit                          (exitFailure)
import           System.IO                            (hPutStrLn, stderr)

main :: IO ()
main = do
  url    <- requireEnv "DATABASE_URL"
  migDir <- requireEnv "MIGRATIONS_DIR"
  conn   <- connectPostgreSQL (BC8.pack url)
  result <- withTransaction conn $ do
    _ <- runMigration $ MigrationContext MigrationInitialization True conn
    runMigration $ MigrationContext (MigrationDirectory migDir) True conn
  close conn
  case result of
    MigrationSuccess -> putStrLn "migrations: ok"
    MigrationError e -> hPutStrLn stderr ("migration error: " <> e) >> exitFailure

requireEnv :: String -> IO String
requireEnv k = do
  mv <- lookupEnv k
  case mv of
    Just v  -> pure v
    Nothing -> do
      hPutStrLn stderr ("missing env: " <> k)
      exitFailure
