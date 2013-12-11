{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models where

import qualified Data.ByteString.Char8 as BS
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import GHC.Generics (Generic)

data CommitInfo = CommitInfo {
    message :: BS.ByteString
    } deriving (Show, Generic)

data Commit = Commit {
    sha :: BS.ByteString,
    url :: BS.ByteString,
    commit :: CommitInfo
    } deriving (Show, Generic)

instance FromJSON Commit
instance FromJSON CommitInfo
