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
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
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
    , pacPath :: Text
    , pacBody :: Text
    }

maxBytes :: Int
maxBytes = 65535 * 8

textToLBS :: Text -> ByteStringLazy.ByteString
textToLBS = ByteStringLazy.fromStrict . encodeUtf8

createManager :: IO Manager
createManager =
    newManager $ Client.defaultManagerSettings

createDestReq :: Text -> Int -> Wai.Request -> Text -> Client.Request
createDestReq dhost dport req body =
    let
        path = httpRequestPath req
        url = "http://" <> dhost <> ":" <> (textShow dport) <> "/" <> path
        method = Wai.requestMethod req
        headers = Wai.requestHeaders req
        xhost = (encodeUtf8 . textShow . Wai.remoteHost) req
    in
        (parseRequest_ . unpack $ url)
                { Client.method = method
                , Client.requestHeaders = ("X-Forwarded-For", xhost) : headers
                , Client.requestBody = (Client.RequestBodyLBS . textToLBS) body
                }

destHostAndPort :: Wai.Request -> (Text, Int)
destHostAndPort req =
    let
        hostfull = (textDecodeUtf8 . fromJust . Wai.requestHeaderHost) req
        host = Text.takeWhile sfun hostfull
        tport = Text.takeWhileEnd sfun hostfull
        port = if Text.length tport < Text.length hostfull then
            (read . unpack) tport :: Int
        else
            80
    in
        (host, port)
    where
        sfun ch = ':' /= ch

proxyServerStart :: ProxyServerOptions -> IO (MVar.MVar ())
proxyServerStart da = do
    let ProxyServerOptions
            { status
            , input
            , forwarded
            , output
            , host
            , port
--             , pacPath
            , pacBody
            } = da
    man <- createManager
    handle <- MVar.newEmptyMVar
    lock <- MVar.newMVar ()
    status $ "Starting server, host: " <> host <> " port: " <> (textShow port)
    serverRunBackground status handle host port $ \req respond -> do
        MVar.withMVar lock $ \_ -> do
            let (destHost, destPort) = destHostAndPort req
            if {- host == destHost && -} port == destPort || destPort == 443 then do
                if port == destPort then do
                    status $ "Request to proxy itself, returning PAC"
                    respond $ responseLBS HTTPTypes.status200 [] $ textToLBS $
                        pacBody
                else do
                    status $ "Ignoring request to port 443"
                    respond $ responseLBS HTTPTypes.status200 [] $ textToLBS $
                        ""
            else do
                status $ "Forwarding request, host [" <> destHost <> "], port: [" <> (textShow destPort) <> "]"
                -- input
                body <- httpRequestBodyText req
                input $ serverFormatReq req
                input body
                catch (do
                        -- forwarded
                        let dreq = createDestReq destHost destPort req body
                        forwarded $ serverFormatDestReq dreq
                        forwarded body
                        -- output
                        (dresp, dbody) <- withResponse dreq man $ \resp -> do
                            dbody <- httpResponseBody (textShow req) resp maxBytes
                            return (resp, dbody)
                        output $ serverFormatResponse dresp
--                         output dbody
                        respond $ responseLBS HTTPTypes.status200 [] $
                            dbody )
                    (\(_::SomeException) -> do
                        status $ "Forwarding error, ignoring it"
                        respond $ responseLBS HTTPTypes.status200 [] $ textToLBS $
                            "" )
    return handle

proxyServerStop :: (MVar.MVar ()) -> IO ()
proxyServerStop handle =
    MVar.putMVar handle ()
