{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Options where

import RIO
import Options.Applicative
import Lens.Micro.TH
import Web.HttpApiData
import RIO.Text

data SearchOpts = SearchOpts 
    { _query :: !Text
    , _sort :: !SortOrder
    --, nps      :: Range Float
    --, stars    :: Range Float
    --, duration :: Range Float
    --, rating   :: Range Float
    , _chroma :: !Bool
    , _noodle :: !Bool
    , _ranked :: !Bool
    , _cinema :: !Bool
    , _curated :: !Bool
    , _ai :: !AiOptions
    --, sort     :: SortOrder
    } deriving (Show)


data Range n = Range { _min :: Maybe n, _max :: Maybe n } deriving Show


data AiOptions = Both | OnlyAi | NoAi deriving (Eq, Enum, Show)

instance ToHttpApiData AiOptions where
    toQueryParam a = case a of
        Both -> "true"
        OnlyAi -> "false"
        NoAi -> "null"


data SortOrder = Latest | Relevance | Rating | Curated  deriving (Eq, Enum, Show, Read)

instance ToHttpApiData SortOrder where
    toQueryParam = pack . show


parseSearchOpts :: ParserInfo SearchOpts
parseSearchOpts = info (helper <*> _parseSearchOpts)
    (  fullDesc
    <> progDesc "Search for a map on Beatsaver and preview it"
    <> header "b-line" )

_parseSearchOpts :: Parser SearchOpts
_parseSearchOpts = SearchOpts 
    <$> strOption
        (  long "search"
        <> short 's'
        <> help "What text to search for"
        <> metavar "STRING")
    <*> option auto
        (  long "order"
        <> short 'o'
        <> value Relevance
        <> help "What order to sort results in"
        <> metavar "STRING" ) --todo: should this list enum values?
    <*> switch 
        (  short 'c'
        <> help "Filter for Chroma maps")
    <*> switch 
        (  short 'n'
        <> help "Filter for Noodle maps")
    <*> switch 
        (  short 'r'
        <> help "Filter for ranked maps")
    <*> switch 
        (  short 'm'
        <> help "Filter for cinema maps")
    <*> switch 
        (  short 'v'
        <> help "Filter for curated maps")
    <*> (   flag' OnlyAi 
            (  short 'a'
            <> help "Allow ONLY AI generated maps") 
        <|> flag' NoAi 
            (  short 'A'
            <> help "Don't allow AI generated maps") 
        <|> pure Both) 

makeLenses ''SearchOpts
makeLenses ''Range

class HasSearchOpts a where
    searchOptsL :: Lens' a SearchOpts