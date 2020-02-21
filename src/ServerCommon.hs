--
-- Copyright 2020, akashche at redhat.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module ServerCommon
    ( serverRunBackground
    , serverFormatReq
    , serverFormatDestReq
    , serverFormatResponse
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.String as String
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

formatHeaders :: [HTTPTypes.Header] -> Text
formatHeaders headers =
    foldl' hfun "" headers
    where
        uncase = textDecodeUtf8 . CaseInsensitive.original
        hfun ac (name, val) = ac <> (uncase name) <> " : " <> (textDecodeUtf8 val) <> "\n"

-- https://stackoverflow.com/a/45846292/314015
serverRunBackground :: (Text -> IO ()) -> MVar.MVar () -> Text -> Int -> Wai.Application -> IO ()
serverRunBackground status handle host port app = do
    _ <- Concurrent.forkOS $
        Async.race_ (MVar.takeMVar handle) $ do
            _ <- catch ( do
                    let wh = String.fromString $ unpack host
                    let ds = Warp.defaultSettings
                    let settings = Warp.setHost wh $ Warp.setPort port ds
                    Warp.runSettings settings app )
                (\(e::SomeException) -> status $ "Server stopped, message: [" <> (textShow e) <> "]")
            return ()
    return ()

serverFormatReq :: Wai.Request -> Text
serverFormatReq req =
    let
        xhost = (textShow . Wai.remoteHost) req
        path = httpRequestPath req
        method = (textShow . Wai.requestMethod) req
        headers = (formatHeaders . Wai.requestHeaders) req
    in
        xhost <> "\n" <> method <> " " <> path <> "\n" <> headers

serverFormatDestReq :: Client.Request -> Text
serverFormatDestReq req =
    let
        method = (textShow . Client.method) req
        host = (textDecodeUtf8 . Client.host) req
        port = Client.port req
        path = (textDecodeUtf8 . Client.path) req
        headers = (formatHeaders . Client.requestHeaders) req
        dest = host <> ":" <> (textShow port)
    in
        dest <> "\n" <> method <> " " <> path <> "\n" <> headers

serverFormatResponse :: forall a . Client.Response a -> Text
serverFormatResponse resp =
    let
        code = (textShow . HTTPTypes.statusCode . Client.responseStatus) resp
        msg = (textDecodeUtf8 . HTTPTypes.statusMessage . Client.responseStatus) resp
        headers = (formatHeaders . Client.responseHeaders) resp
    in
        code <> " " <> msg <> "\n" <> headers