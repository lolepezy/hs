{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rest where
 
import Control.Monad    (msum)
import Control.Monad.Trans   (MonadIO(liftIO))
import Happstack.Server ( Method(GET, POST), Response, ServerPart, dir, method, toResponse
                        , nullConf, ok, simpleHTTP, seeOther, path
                        )
import Text.JSON.Generic
import Resources
import Database.PostgreSQL.Simple
import Data.Pool (createPool, withResource)


 
{--
main :: IO ()
main = simpleHTTP nullConf $ msum
       [ do dir "member-resources" $ path $ \memberId -> ok $ do
            pool <- createPool createConnection close 1 10 5
            resources <- getMemberResources memberId
            return resources
       , do dir "all-resources" $ ok $ do 
            pool <- createPool createConnection close 1 10 5
            resources <- getAllResources pool
            return resources 
       ]
--}


allResourcesJSON :: ServerPart Response
allResourcesJSON = do 
  pool <- liftIO $ createPool createConnection close 1 10 5
  resources <- liftIO $ getAllResources pool
  return $ toResponse resources 


main :: IO ()
main = simpleHTTP nullConf $ msum 
  [ dir "all-resources" $ allResourcesJSON ]

