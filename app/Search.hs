{-# LANGUAGE NoImplicitPrelude #-}
module Search where

import qualified Brick as B
import RIO
import Types
import Network.HTTP.Req
import Web

searchUi :: B.App s e n
searchUi = B.App {
    B.appDraw = undefined,
    B.appChooseCursor = undefined,
    B.appHandleEvent = undefined,
    B.appStartEvent = undefined,
    B.appAttrMap = undefined
}

search :: (HasSearchOpts env) => RIO env a
search = do
    opts <- view searchOptsL
    --todo: multi page stuff with brick
    res <- quickGet (bsMapSearchUrl 0) (Proxy :: Proxy (JsonResponse BsSearchRes)) (bsMapSearchOpts opts)
    let (BsSearchRes infos) = responseBody res
    pure undefined



    --steps:
    --send api req to beatsaver with query options
    --convert (stream?) json to list of search results
    --display list of search results with page up/down 

    --OR
    --initialize infinite scroll brick app
    --lazily and asynchronously send requests to beatsaver when needed
        -- -> way to avoid that being a memory hog? something something caching in brick app state as list of page chunks

