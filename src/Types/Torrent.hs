{-# LANGUAGE OverloadedStrings #-}

module Types.Torrent (
fromJson,
QueryResult(..),
Torrent(..)
) where

import Data.Aeson((.:), (.:?))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.DateTime as Date
import qualified Data.Map.Strict as Map
import Control.Monad (mzero)


data QueryResult = QueryResult
    { query :: String
    , total :: Int
    , offset :: Int
    , limit :: Int
    , torrents :: [Torrent]
    } deriving (Show)


data IntermediateQueryResult = IntermediateQueryResult
    { query_ :: String
    , total_ :: String
    , offset_ :: String
    , limit_ :: String
    , torrents_ :: [IntermediateTorrent]
    }


instance Json.FromJSON IntermediateQueryResult where
    parseJSON (Json.Object v) =
        IntermediateQueryResult <$> v .:  "query"
                                <*> v .:  "total"
                                <*> v .:  "offset"
                                <*> v .:  "limit"
                                <*> v .:  "torrents"
    parseJSON _ = mzero


data Torrent = Torrent
    { id :: Int
    , name :: String
    , category :: Int
    , seeders :: Int
    , leechers :: Int
    , comments :: Int
    , isVerified :: Bool
    , added :: Date.DateTime
    , size :: Int
    , times_completed :: Int
    , owner :: Int
    , categoryName :: String
    , categoryImage :: String
    , username :: String
    , privacy :: String
    } deriving (Show)


data IntermediateTorrent = IntermediateTorrent
    { id_ :: String
    , name_ :: String
    , category_ :: String
    , seeders_ :: String
    , leechers_ :: String
    , comments_ :: String
    , isVerified_ :: String
    , added_ :: String
    , size_ :: String
    , times_completed_ :: String
    , owner_ :: String
    , categoryName_ :: String
    , categoryImage_ :: String
    , username_ :: String
    , privacy_ :: String
    }


instance Json.FromJSON IntermediateTorrent where
    parseJSON (Json.Object v) =
        IntermediateTorrent <$> v .:  "id"
                            <*> v .:  "name"
                            <*> v .:  "category"
                            <*> v .:  "seeders"
                            <*> v .:  "leechers"
                            <*> v .:  "comments"
                            <*> v .:  "isVerified"
                            <*> v .:  "added"
                            <*> v .:  "size"
                            <*> v .:  "times_completed"
                            <*> v .:  "owner"
                            <*> v .:  "categoryname"
                            <*> v .:  "categoryimage"
                            <*> v .:  "username"
                            <*> v .:  "privacy"
    parseJSON _ = mzero


fromJson :: String -> Either String QueryResult
fromJson string =
    case Json.eitherDecode . C.pack $ string :: Either String IntermediateQueryResult of
        Left error -> Left error
        Right result -> Right $ queryFromIntermediate result


queryFromIntermediate :: IntermediateQueryResult -> QueryResult
queryFromIntermediate intermediate =
    let makeTorrent i =
            Torrent (read $ id_ i)
                    (name_ i)
                    (read $ category_ i)
                    (read $ seeders_ i)
                    (read $ leechers_ i)
                    (read $ comments_ i)
                    (if isVerified_ i == "0" then False else True)
                    (read $ added_ i)
                    (read $ size_ i)
                    (read $ times_completed_ i)
                    (read $ owner_ i)
                    (categoryName_ i)
                    (categoryImage_ i)
                    (username_ i)
                    (privacy_ i)
    in QueryResult (query_ intermediate)
                   (read $ total_ intermediate)
                   (read $ offset_ intermediate)
                   (read $ limit_ intermediate)
                   (map makeTorrent $ torrents_ intermediate)

