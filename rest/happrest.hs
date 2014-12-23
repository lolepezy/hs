{-# LANGUAGE DeriveDataTypeable #-}
module Rest where
 
import Control.Monad    (msum)
import Control.Monad.Trans   (MonadIO(liftIO))
import Happstack.Server ( Method(GET, POST), dir, method
                        , nullConf, ok, simpleHTTP, seeOther, path
                        )
import Text.JSON.Generic
import Resources
 
main :: IO ()
main = simpleHTTP nullConf $ msum
       [ dir "member-resources" $ path $ \memberId -> do
			resources <- liftIO $ getMemberResources memberId
			ok $ resources
       , dir "all-resources" $ do 
			resources <- liftIO getAllResources 
			ok $ resources 
       ]

