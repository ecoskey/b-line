{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import RIO
import Types
import Search

app :: RIO App ()
app = do
    ctx <- view appProcessContext
    mapChoice <- search
    pure ()
