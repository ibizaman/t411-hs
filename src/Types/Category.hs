{-# LANGUAGE OverloadedStrings #-}

module Types.Category (
fromJson,
Category(id, name, children),
) where

import Prelude hiding (id)
import Data.Aeson((.:), (.:?))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.DateTime as Date
import qualified Data.Map.Strict as Map
import Control.Monad (mzero)


fromJson :: String -> Either String [Category]
fromJson string =
    case Json.eitherDecode . C.pack $ string :: Either String (Map.Map String IntermediateCategory) of
        Left error -> Left error
        Right intermediate -> Right . categoriesFromIntermediate $ intermediate


categoriesFromIntermediate :: (Map.Map String IntermediateCategory) -> [Category]
categoriesFromIntermediate intermediate =
    let cats = Map.elems intermediate
        makeCat (IntermediateCategory id name childrenMap) =
            Category (read id :: Int) name (categoriesFromIntermediate . maybeToMap $ childrenMap)
    in map makeCat cats


data Category = Category
    { id :: Int
    , name :: String
    , children :: [Category]
    }


data IntermediateCategory = IntermediateCategory
    { id_ :: String
    , name_ :: String
    , children_ :: Maybe (Map.Map String IntermediateCategory)
    }


maybeToMap :: Maybe (Map.Map k v) -> (Map.Map k v)
maybeToMap Nothing = Map.empty
maybeToMap (Just map) = map


instance Json.FromJSON IntermediateCategory where
    parseJSON (Json.Object v) =
        IntermediateCategory <$> v .:  "id"
                             <*> v .:  "name"
                             <*> v .:? "cats"
    parseJSON _ = mzero


instance Show Category where
    show c =
        let indent = map (unlines . map (\l -> "  " ++ l) . lines)
            childrenString = case length (children c) of
                0 -> ""
                _ -> "\n[\n" ++ (unlines .indent . map show $ (children c)) ++ "]"
        in "Category: " ++ (show $ id c) ++ " " ++ (name c) ++ childrenString

