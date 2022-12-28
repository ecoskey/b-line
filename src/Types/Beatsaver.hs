{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Beatsaver where

import RIO
import Data.Aeson
import Web.HttpApiData (ToHttpApiData)

type SongID = Text

newtype BsSearchRes = BsSearchRes [BsSongInfo]

newtype MapId = MapId Text deriving (Eq, Show, ToHttpApiData, FromJSON)

instance FromJSON BsSearchRes where
    parseJSON = withObject "BsSearchRes" $ \v -> BsSearchRes <$> v .: "docs"

data BsSongInfo = BsSongInfo
    { _id :: !MapId
    , _songName :: !Text
    , _songSubname :: !Text
    , _levelAuthorName :: !Text
    , _songAuthorName :: !Text
    , _isQualified :: !Bool
    , _isRanked :: !Bool
    , _aiMapped :: !Bool
    } deriving Show

instance FromJSON BsSongInfo where
    parseJSON = withObject "BsSongInfo" $ \v -> BsSongInfo
        <$> v .: "id"
        <*> v .: "name"
        <*> (v .: "metadata" >>= (.: "songSubName"))
        <*> (v .: "metadata" >>= (.: "levelAuthorName"))
        <*> (v .: "metadata" >>= (.: "songAuthorName"))
        <*> v .: "qualified"
        <*> v .: "ranked"
        <*> v .: "automapper"
