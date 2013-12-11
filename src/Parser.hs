{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Network.HTTP.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans
import Data.Aeson (decode)

import Models

main = do
    manager <- newManager def
    req <- parseUrl "https://api.github.com/repos/erthalion/vimrc/commits"
    let headers = requestHeaders req
        req' = req {
          requestHeaders = ("User-agent", "some-app") :
                           headers
        }
    runResourceT $ do
        res <- http req' manager
        responseBody res $$+- CB.lines =$ parserSink

printCommits :: [Models.Commit] -> IO ()
printCommits (x:xs) = do
        BS.putStrLn (Models.url x)
        BS.putStrLn (Models.message (commit x))
        printCommits xs
printCommits [] = return ()


parseCommits :: BS.ByteString -> Sink BS.ByteString (ResourceT IO) ()
parseCommits rawData = do
        let parsedData = decode $ BL.fromChunks [rawData] :: Maybe [Models.Commit]
        case parsedData of
            Nothing -> liftIO $ BS.putStrLn "Parse error"
            Just commits -> liftIO $ printCommits commits


-- parsing json from file may be like this 
-- file = BL.readFile "test.json" 
-- (decode <$> file) :: IO (Maybe [Commit])
parserSink :: Sink BS.ByteString (ResourceT IO) ()
parserSink = do
    md <- await
    case md of
        Nothing -> return ()
        Just d -> parseCommits d
