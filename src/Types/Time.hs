{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types.Time
    ( TimeType(..)
    , Time
    , time, toSecs, toBeats
    ) where

import RIO

import Data.Aeson (FromJSON)

data TimeType = Beats | Secs

newtype Time (t :: TimeType) = Time Float
    deriving (Eq, Ord, Enum, Num, Floating, Fractional, Real, RealFloat, RealFrac, FromJSON)

time :: Float -> Time a
time = Time

toSecs :: Float -> Time 'Beats -> Time 'Secs
toSecs bpm (Time t) = Time (t / bpm / 60)

toBeats :: Float -> Time 'Secs -> Time 'Beats
toBeats bpm (Time t) = Time (t * 60 * bpm)