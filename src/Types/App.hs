{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.App where
    
import RIO.Process
import Types.Options
import Lens.Micro.TH

data App = App
  { _appSearchOpts :: !SearchOpts
  , _appProcessContext :: !ProcessContext
  }

makeLenses ''App

--instance HasLogFunc App where
--  logFuncL = appLogFunc
instance HasProcessContext App where
  processContextL = appProcessContext

instance HasSearchOpts App where
  searchOptsL = appSearchOpts