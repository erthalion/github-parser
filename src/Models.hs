{-# LANGUAGE OverloadedStrings #-}
module Models where

import qualified Data.ByteString.Char8 as BS
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))

data CommitInfo = CommitInfo {
    message :: BS.ByteString
    } deriving (Show)

data Commit = Commit {
    sha :: BS.ByteString,
    url :: BS.ByteString,
    commit :: CommitInfo
    } deriving (Show)

instance FromJSON Commit where
    parseJSON (Object v) =
        Commit         <$>
        (v .: "sha")   <*>
        (v .: "url")   <*>
        (v .: "commit")

instance FromJSON CommitInfo where
    parseJSON (Object v) =
        CommitInfo       <$>
        (v .: "message")
