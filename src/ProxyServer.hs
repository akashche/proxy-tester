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
import qualified Network.Wai as Wai

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

textToLBS :: Text -> ByteStringLazy.ByteString
textToLBS = ByteStringLazy.fromStrict . encodeUtf8

createManager :: IO Manager
createManager =
    newManager $ TLS.tlsManagerSettings

createDestReq :: Text -> Int -> Wai.Request -> Text -> Client.Request
createDestReq dhost dport req body =
    let
        path = httpRequestPath req
        url = "http://" <> dhost <> ":" <> (textShow dport) <> path
        method = Wai.requestMethod req
        headers = Wai.requestHeaders req
        xhost = (encodeUtf8 . textShow . Wai.remoteHost) req
    in
        (parseRequest_ . unpack $ url)
                { Client.method = method
                , Client.requestHeaders = ("X-Forwarded-For", xhost) : headers
                , Client.requestBody = (Client.RequestBodyLBS . textToLBS) body
                }

proxyServerStart :: ProxyServerOptions -> IO (MVar.MVar ())
proxyServerStart da = do
    let ProxyServerOptions
            { status
            , input
            , forwarded
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
        body <- httpRequestBodyText req
        input $ serverFormatReq req
        input body
        -- forwarded
        let dreq = createDestReq destHost destPort req body
        forwarded $ serverFormatDestReq dreq
        forwarded body
        -- output
        (dresp, dbody) <- withResponse dreq man $ \resp -> do
            dbody <- httpResponseBodyText (textShow req) resp maxBytes
            return (resp, dbody)
        output $ serverFormatResponse dresp
        output dbody
        respond $ responseLBS HTTPTypes.status200 [] $ textToLBS $
            dbody
    return handle

proxyServerStop :: (MVar.MVar ()) -> IO ()
proxyServerStop handle =
    MVar.putMVar handle ()
