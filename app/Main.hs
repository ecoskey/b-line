{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import RIO
import App
import RIO.Process
import Types.App
import Options.Applicative
import Types

main :: IO ()
main = do
    options <- execParser parseSearchOpts
    ctx <- mkDefaultProcessContext
    let env = App options ctx
    runRIO env app