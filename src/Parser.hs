{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Network.HTTP.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans
import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Text (Text)

import Models

main = do
  manager <- newManager def
  req <- parseUrl "https://api.github.com/repos/erthalion/vimrc/commits"
  let headers = requestHeaders req
      req' = req {
          requestHeaders = ("User-agent", "erthalion") :
                           headers
        }
  runResourceT $ do
    res <- http req' manager
    responseBody res $$+- CB.lines =$ counterSink

printCommits :: [Models.Commit] -> IO ()
printCommits (x:xs) = do
                    BS.putStrLn (Models.url x)
                    BS.putStrLn (Models.message (commit x))
                    printCommits xs
printCommits [] = return ()

-- parsing json from file may be like this 
-- file = BL.readFile "test.json" 
-- (decode <$> file) :: IO (Maybe [Commit])
counterSink :: Sink BS.ByteString (ResourceT IO) ()
counterSink = do
  md <- await
  case md of
    Nothing -> return ()
    Just d -> do
      liftIO $ BS.putStrLn "--------"
      case (decode (BL.fromChunks [d]) :: Maybe [Models.Commit]) of
        Nothing -> do
            liftIO $ BS.putStrLn "Parse error"
        Just commits -> do
            liftIO $ printCommits commits
