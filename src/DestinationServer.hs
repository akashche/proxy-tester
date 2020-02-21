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
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Network.HTTP.Types as HTTPTypes

import ServerCommon

data DestinationServerOptions = DestinationServerOptions
    { status :: Text -> IO ()
    , input :: Text -> IO ()
    , output :: Text -> IO ()
    , host :: Text
    , port :: Int
    , response :: Text
    }

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
    serverRunBackground status handle host port $ \req respond -> do
        rt <- httpRequestBodyText req
        input $ serverFormatReq req
        input rt
        output response
        respond $ responseLBS HTTPTypes.status200 [] $ ByteStringLazy.fromStrict $ encodeUtf8 $
            response
    return handle

destinationServerStop :: (MVar.MVar ()) -> IO ()
destinationServerStop handle =
    MVar.putMVar handle ()
