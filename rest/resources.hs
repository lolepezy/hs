{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Resources where

import System.IO
import qualified Data.ByteString as B
import Data.Time
import Data.Time.Format
import Text.JSON
import Text.JSON.Generic
import System.Locale (defaultTimeLocale)

import Control.Monad
import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Data.Pool
import Data.Pool (createPool, withResource)


formatDate :: UTCTime -> String
formatDate utcTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" utcTime

createConnection = connect defaultConnectInfo { connectDatabase = "custdb" }

{-
data Resource = Ipv4Assignment {
	ass_range :: String,
	ass_date :: Maybe String, 
	ass_status :: Maybe String, 
	ass_type :: Maybe String, 
	irstatus :: Maybe String
} | Ipv4Allocation {
	alloc_range :: String, 
	alloc_date :: Maybe String, 
	alloc_status :: Maybe String, 
	alloc_type :: Maybe String
} deriving (Show, Data, Typeable)
-}


data Ipv4Assignment = Ipv4Assignment {
	ass_range :: String,
	ass_date :: Maybe String, 
	ass_status :: Maybe String, 
	ass_type :: Maybe String, 
	irstatus :: Maybe String
}  deriving (Show, Data, Typeable)


instance FromRow Ipv4Assignment where
     fromRow = Ipv4Assignment <$> field <*> field <*> field <*> field <*> field

--instance FromRow Ipv4Allocation where
--     fromRow = Ipv4Allocation <$> field <*> field <*> field <*> field

{-
      select(entityManager, s"""
          |  SELECT e FROM ${entityType.getSimpleName} e
          |   WHERE e.resourceStatusInfo IS NOT NULL
          |     AND e.resourceStatusInfo.resourceStatus IN ('$ALLOCATED', '$ASSIGNED', '$ISSUED')
          |ORDER BY e.membershipId, e.resourceStart
          |""".stripMargin) 
-}

fetchAllResources :: Pool Connection -> IO [Ipv4Assignment]
fetchAllResources pool = do
  let sql1 = "SELECT * FROM resourcedb.Ipv4AssignmentResource WHERE 1=1"
  withResource pool $ \conn -> do
	xs :: [Ipv4Assignment] <- query conn sql1 ()
	return $ xs

fetchMemberResources :: Integer -> IO [Ipv4Assignment]
fetchMemberResources memberId = do 
	c :: UTCTime <- getCurrentTime
	let f = formatDate c
	return $ [Ipv4Assignment { 
      ass_range = "1.1.1.1/32", 
      ass_date = Just f, 
      ass_status = Just "Some status", 
      ass_type = Just "Some type", 
      irstatus = Just "Squeezed" }]


getMemberResources :: Integer -> IO String
getMemberResources memberId = do 
	resources <- fetchMemberResources memberId
	return $ encodeJSON resources

getAllResources :: Pool Connection -> IO String
getAllResources pool = do 
	resources <- fetchAllResources pool
	return $ encodeJSON resources

