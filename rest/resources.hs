{-# LANGUAGE OverloadedStrings #-}
module Resources where

import System.IO
import qualified Data.ByteString as B

getMemberResources :: Integer -> IO String
getAllResources :: IO String

getMemberResources memberId = do 
	return $ "{ \"resources\": \"" ++ show(memberId) ++ "\"}" 

getAllResources = do 
	return $ "{ \"all-resources\" : \"\" }"
	-- get that shit from the DB
	
	-- serialize it


