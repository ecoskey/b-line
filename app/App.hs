{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import RIO

app :: RIO SimpleApp ()
app = do
  logInfo "We're inside the application!"
