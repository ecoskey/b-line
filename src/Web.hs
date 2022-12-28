{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Web where

import Network.HTTP.Req
import RIO
import Types.Options

quickReq ::
    ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
    , MonadIO m, HttpMethod method, HttpBody body, HttpResponse res ) =>
    method -> Url scheme -> body -> Proxy res -> Option scheme -> m res
quickReq method url body proxy opts =
    runReq defaultHttpConfig $ req method url body proxy opts

quickGet :: (MonadIO m, HttpResponse res) => Url scheme -> Proxy res -> Option scheme -> m res
quickGet url = quickReq GET url NoReqBody

bsMapSearchUrl :: Int -> Url 'Https
bsMapSearchUrl n = https "beatsaver.com" /: "search" /: "text" /~ n

bsMapSearchOpts :: SearchOpts -> Option 'Https
bsMapSearchOpts (SearchOpts q o ch n r ci cu a) = 
       "q" =: q
    <> "sortOrder" =: o
    <> "chroma" =: ch
    <> "noodle" =: n
    <> "ranked" =: r
    <> "cinema" =: ci
    <> "curated" =: cu
    <> "automapper" =: a