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

module DestinationServer
    ( DestinationServerOptions(..)
    , destinationServerStart
    , destinationServerStop
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.String as String
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

data DestinationServerOptions = DestinationServerOptions
    { status :: Text -> IO ()
    , input :: Text -> IO ()
    , output :: Text -> IO ()
    , host :: Text
    , port :: Int
    , response :: Text
    }

-- https://stackoverflow.com/a/45846292/314015
runServerBackground :: (Text -> IO ()) -> MVar.MVar () -> Text -> Int -> Wai.Application -> IO ()
runServerBackground status handle host port app = do
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

destinationServerStart :: DestinationServerOptions -> IO (MVar.MVar ())
destinationServerStart da = do
    let DestinationServerOptions
            { status
            , input
            , output
            , host
            , port
            , response
            } = da
    handle <- MVar.newEmptyMVar
    status $ "Starting server, host: " <> host <> " port: " <> (textShow port)
    runServerBackground status handle host port $ \req respond -> do
        rt <- httpRequestBodyText req
        input (httpRequestPath req)
        input rt
        output response
        respond $ responseLBS HTTPTypes.status200 [] $ ByteStringLazy.fromStrict $ encodeUtf8 $
            response
    return handle

destinationServerStop :: (MVar.MVar ()) -> IO ()
destinationServerStop handle =
    MVar.putMVar handle ()
