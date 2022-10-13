{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Types.SongData where

import RIO
import RIO.Partial
import Path
import Lens.Micro.TH
import Data.Aeson
import Types.Time
import Data.Aeson.Types


data SongInfo = SongInfo -- add version field
    { _name           :: !Text
    , _subName        :: !Text
    , _author         :: !Text
    , _levelAuthor    :: !Text
    , _bpm            :: !Float
    , _shuffle        :: !(Time 'Beats)
    , _shufflePeriod  :: !(Time 'Beats)
    , _songFilename   :: !(Path Rel File) -- replace with Path Rel File from path package
    , _songTimeOffset :: !(Time 'Secs)
    , _difficultySets :: [DifficultySet]
    } deriving (Eq, Ord)

instance FromJSON SongInfo where
    parseJSON = withObject "SongInfo" $ \v -> SongInfo
        <$> v .: "_songName"
        <*> v .: "_songSubName"
        <*> v .: "_songAuthorName"
        <*> v .: "_levelAuthorName"
        <*> v .: "_beatsPerMinute"
        <*> v .: "_shuffle"
        <*> v .: "_shufflePeriod"
        <*> v .: "_songFileName"
        <*> v .: "_songTimeoffset"
        <*> v .: "_difficultySets"


data DifficultySet = DifficultySet 
    { _characteristic :: !Characteristic
    , _difficulties   :: [BeatmapInfo]
    } deriving (Eq, Ord)

instance FromJSON DifficultySet where
    parseJSON = withObject "DifficultySet" $ \v -> DifficultySet
        <$> v .: "_beatmapCharacteristicName"
        <*> v .: "_difficultyBeatmaps"


data Characteristic = Standard | NoArrows | OneSaber deriving (Eq, Bounded, Ord, Enum)

instance FromJSON Characteristic where
    parseJSON = withText "Characteristic" $ \case
        "Standard" -> pure Standard
        "NoArrows" -> pure NoArrows
        "OneSaber" -> pure OneSaber
        _          -> fail "Invalid beatmap characteristic name."


data BeatmapInfo = BeatmapInfo
    { _difficulty  :: Difficulty
    , _beatmapPath :: !(Path Rel File) -- replace with Path Rel File from path package
    , _njs         :: !Float
    , _offset      :: !Float
    } deriving (Eq, Ord)

instance FromJSON BeatmapInfo where
    parseJSON = withObject "BeatmapInfo" $ \v -> BeatmapInfo
        <$> v .: "_difficultyRank"
        <*> v .: "_beatmapFilename"
        <*> v .: "_noteJumpMovementSpeed"
        <*> v .: "_noteJumpStartBeatOffset"


data Difficulty = E | N | H | X | XP deriving (Eq, Ord, Enum)

instance Show Difficulty where
    show d = case d of
        E -> "Easy"
        N -> "Normal"
        H -> "Hard"
        X -> "Expert"
        XP -> "ExpertPlus"

instance FromJSON Difficulty where
    parseJSON v = do
        i <- parseJSON v
        pure . toEnum $ (i - 1) `div` 2


data Beatmap = Beatmap
    { _frames     :: Map (Time 'Beats) Frame
    , _walls      :: Map (Time 'Beats) Wall
    --, _bpmChanges :: Map (Time 'Beats) Float 
    -- note: add bpm change suppport
    } deriving (Eq, Ord)


data Frame = Frame 
    { _c0 :: Bloq, _c1 :: Bloq, _c2 :: Bloq, _c3 :: Bloq
    , _b0 :: Bloq, _b1 :: Bloq, _b2 :: Bloq, _b3 :: Bloq
    , _a0 :: Bloq, _a1 :: Bloq, _a2 :: Bloq, _a3 :: Bloq
    } deriving (Eq, Ord)

data Bloq = Note !NoteColor !NoteDir | Bomb | NoBloq deriving (Eq, Ord)


data JsonNote = JsonNote !(Time 'Beats) !Column !Row !NoteColor !NoteDir deriving (Eq, Ord)

instance FromJSON JsonNote where
    parseJSON = withObject "JsonNote" $ \v -> JsonNote
        <$> v .: "b"
        <*> v .: "x"
        <*> v .: "y"
        <*> v .: "c"
        <*> v .: "d"


data JsonBomb = JsonBomb !(Time 'Beats) !Column !Row deriving (Eq, Ord)

instance FromJSON JsonBomb where
    parseJSON = withObject "JsonBomb" $ \v -> JsonBomb
        <$> v .: "b"
        <*> v .: "x"
        <*> v .: "y"


data NoteColor = Red | Blue deriving (Eq, Bounded, Ord, Enum)

instance FromJSON NoteColor where
    parseJSON v = toEnum <$> parseJSON v


data NoteDir = U | D | L | R | UL | UR | DL | DR | Dot deriving (Eq, Bounded, Ord, Enum)

instance FromJSON NoteDir where
    parseJSON v = toEnum <$> parseJSON v


data Wall = Wall !(Time 'Beats) !(Time 'Beats) !Column !Column !Row deriving (Eq, Ord)

instance FromJSON Wall where
    parseJSON = withObject "Wall" $ \v -> do
        t <- v .: "b"
        d <- v .: "d"
        x <- v .: "x" :: Parser Int
        w <- v .: "w" :: Parser Int
        let c1 = toEnum x
            c2 = toEnum (x + w)
        y <- v .: "y"
        pure $ Wall t d c1 c2 y
        

data Column = C0 | C1 | C2 | C3 deriving (Eq, Bounded, Ord, Enum)

instance FromJSON Column where
    parseJSON v = toEnum <$> parseJSON v


data Row = R0 | R1 | R2 deriving (Eq, Bounded, Ord, Enum)

instance FromJSON Row where
    parseJSON v = toEnum <$> parseJSON v

-- lenses

makeLenses ''SongInfo
makeLenses ''DifficultySet
makeLenses ''BeatmapInfo
makeLenses ''Beatmap
makeLenses ''Frame

atPos :: (Column, Row) -> Lens' Frame Bloq
atPos pos = case pos of 
    (C0, R0) -> a0
    (C1, R0) -> a1
    (C2, R0) -> a2
    (C3, R0) -> a3
    (C0, R1) -> b0
    (C1, R1) -> b1
    (C2, R1) -> b2
    (C3, R1) -> b3
    (C0, R2) -> c0
    (C1, R2) -> c1
    (C2, R2) -> c2
    (C3, R2) -> c3