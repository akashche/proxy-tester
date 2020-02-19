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

module ProxyServer
    ( ProxyServerOptions(..)
    , proxyServerStart
    , proxyServerStop
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTPTypes

import ServerCommon

data ProxyServerOptions = ProxyServerOptions
    { status :: Text -> IO ()
    , input :: Text -> IO ()
    , forwarded :: Text -> IO ()
    , output :: Text -> IO ()
    , host :: Text
    , port :: Int
    , destHost :: Text
    , destPort :: Int
    }

maxBytes :: Int
maxBytes = 65535

createManager :: IO Manager
createManager =
    newManager $ TLS.tlsManagerSettings

proxyServerStart :: ProxyServerOptions -> IO (MVar.MVar ())
proxyServerStart da = do
    let ProxyServerOptions
            { status
            , input
            , forwarded = _f
            , output
            , host
            , port
            , destHost
            , destPort
            } = da
    man <- createManager
    handle <- MVar.newEmptyMVar
    status $ "Starting server, host: " <> host <> " port: " <> (textShow port)
    serverRunBackground status handle host port $ \req respond -> do
        -- input
        let path = httpRequestPath req
        rt <- httpRequestBodyText req
        input path
        input rt
        -- forwarded
        let url = "http://" <> destHost <> ":" <> (textShow destPort) <> path
        let dreq = (parseRequest_ . unpack $ url)
                { Client.method = "GET"
                , Client.requestHeaders = []
                }
        -- output
        dresp <- withResponse dreq man $ \resp ->
            httpResponseBodyText (textShow req) resp maxBytes
        output dresp
        respond $ responseLBS HTTPTypes.status200 [] $ ByteStringLazy.fromStrict $ encodeUtf8 $
            dresp
    return handle

proxyServerStop :: (MVar.MVar ()) -> IO ()
proxyServerStop handle =
    MVar.putMVar handle ()
