{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad
import Control.Applicative
import Text.JSON
import Text.JSON.Generic
import Data.Pool (createPool, withResource)

data CommandAudit = CommandAudit {
	caId :: Int,
	commandType :: Maybe String,
	commandSummary :: Maybe String
} deriving (Show, Typeable)

instance JSON CommandAudit where
    showJSON ca = makeObj [("id", showJSON $ caId ca),("commandType", showJSON $ commandType ca), ("commandSummary", showJSON $ commandSummary ca)]
    readJSON _ = mzero

instance FromRow CommandAudit where
     fromRow = CommandAudit <$> field <*> field <*> field

main = do
    pool <- createPool (connect defaultConnectInfo { connectDatabase = "certdb" }) close 1 10 5
    withResource pool $ \conn -> do
	  xs :: [CommandAudit] <- query conn [sql| select id, commandtype, commandsummary from commandaudit |]()
	  print $ encode $ map showJSON xs
