{-# LANGUAGE NoImplicitPrelude #-}
module Types.Options where

import RIO
import Types.Beatsaver (SongID)
import Options.Applicative

data Options 
    = Search !Text
    | Info !SongID
    | Play !SongID
    deriving (Eq, Ord)

optionsParser :: Parser Options
optionsParser = optionsParser

commandsParser :: Parser Options
commandsParser = subparser 
    (  command "search" 
        (info searchParser (progDesc "Search for a map on Beatsaver"))
    <> command "info" 
        (info songInfoParser (progDesc "Get the info for a map on Beatsaver")) 
    <> command "play" 
        (info playParser (progDesc "Play a map on Beatsaver")))

searchParser :: Parser Options
searchParser = Info
    <$> option str 
        (  long "query"
        <> short 'q'
        <> metavar "STRING" 
        <> help "What text to search Beatsaver for") 

songInfoParser :: Parser Options
songInfoParser = undefined

playParser :: Parser Options
playParser = undefined