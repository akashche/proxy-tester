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

module Proxy
    ( ProxyAppenders(..)
    , proxyCreateRoot
    , proxyCreateInput
    , proxyCreateForwarded
    , proxyCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import ProxyServer
import UICommon

type ProxyResult = (Ref Group, Text -> IO (), IO ())

data ProxyForm = ProxyForm
    { addr :: Ref Input
    , port :: Ref IntInput
    , pacPath :: Ref Input
    , pacBody :: Ref TextDisplay
    , stop :: Ref Button
    }

data ProxyAppenders = ProxyAppenders
    { input :: Text -> IO ()
    , inputClear :: IO ()
    , forwarded :: Text -> IO ()
    , forwardedClear :: IO ()
    , output :: Text -> IO ()
    , outputClear :: IO ()
    }

startCallback ::(Text -> IO ()) -> ProxyForm -> ProxyAppenders -> Ref Button -> IO ()
startCallback statusAppend form da start = do
    let ProxyForm
            { addr
            , port
            , pacPath
            , pacBody
            , stop
            } = form
    let ProxyAppenders
            { input
            , inputClear
            , forwarded
            , forwardedClear
            , output
            , outputClear
            } = da
    av <- getValue addr
    pv <- (read . unpack) <$> getValue port :: IO Int
    ppv <- getValue pacPath
    pbv <- uiGetTextDisplayValue pacBody
    deactivate start
    let pso = ProxyServerOptions
            { input = input
            , forwarded = forwarded
            , output = output
            , status = statusAppend
            , host = av
            , port = pv
            , pacPath = ppv
            , pacBody = pbv
            }
    server <- proxyServerStart pso
    inputClear
    forwardedClear
    outputClear
    setCallback stop $ \_ -> do
        deactivate stop
        proxyServerStop server
        activate start
        return ()
    activate stop

proxyCreateRoot :: Text -> (Text -> IO ()) -> ProxyAppenders-> IO (Ref Group)
proxyCreateRoot label statusAppend pa = do
    let UIConstants
            { borderSize = bs
            , formRowHeight = frh
            , formLabelWidth = flw
            , formInputWidth = fiw
            , buttonWidth = btw
            , buttonHeight = bth
            } = uiConstants
    let UIRectangles
            { contentRect
            , contentBodyRect
            , formRect
            , buttonsPanelRect
            } = uiRectangles
    let (bx, by, _, _) = fromRectangle contentBodyRect

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- uiCreateHeader "Proxy Server"

    body <- groupNew formRect Nothing
    setResizable gr (Just body)
    form <- groupNew formRect Nothing
    setResizable form (Nothing :: Maybe (Ref Box))

    -- address
    addrLabel <- boxNew (toRectangle (bx, by + bs, flw, frh)) (Just "IP Address")
    uiSetLabelAlign addrLabel
    addrInput <- inputNew (toRectangle (bx + flw + bs, by + bs, fiw, frh)) Nothing Nothing
    _ <- setValue addrInput "0.0.0.0" Nothing

    -- port
    portLabel <- boxNew (toRectangle (bx, by + bs*4, flw, frh)) (Just "TCP Port")
    uiSetLabelAlign portLabel
    portInput <- intInputNew (toRectangle (bx + flw + bs, by + bs*4, fiw, frh)) Nothing
    _ <- setValue portInput "8082" Nothing

    -- pac url
    pacPathLabel <- boxNew (toRectangle (bx, by + bs*7, flw, frh)) (Just "PAC Path")
    uiSetLabelAlign pacPathLabel
    pacPathInput <- inputNew (toRectangle (bx + flw + bs, by + bs*7, fiw, frh)) Nothing Nothing
    _ <- setValue pacPathInput "/*" Nothing
    deactivate pacPathInput

    -- pac body
    pacBodyLabel <- boxNew (toRectangle (bx, by + bs*10, flw, frh)) (Just "PAC Contents")
    uiSetLabelAlign pacBodyLabel
    pacBodyDisp <- textDisplayNew (toRectangle (bx + flw + bs, by + bs*10, fiw*2, frh*8))Nothing
    setTextsize pacBodyDisp (FontSize 12)
    pacBodyBuf <- textBufferNew Nothing Nothing
    setBuffer pacBodyDisp (Just pacBodyBuf)
    uiTextDisplayAppend pacBodyDisp $ "\n" <>
            "function FindProxyForURL(url, host) {\n" <>
            "    return \"PROXY 127.0.0.1:8082\";\n" <>
            "}"
    end pacBodyDisp
    setResizable form (Just pacBodyDisp)

    end form
    end body

    buttonsPanel <- groupNew buttonsPanelRect Nothing
    setBox buttonsPanel EngravedBox
    let (bpx, bpy, bpw, _) = fromRectangle buttonsPanelRect

    butStart <- buttonNew (toRectangle (bpx + bpw - bs*2 - btw*2, bpy + bs, btw, bth)) (Just "Start")
    butStop <- buttonNew (toRectangle (bpx + bpw - bs - btw, bpy + bs, btw, bth)) (Just "Stop")
    deactivate butStop

    let pf = ProxyForm
            { addr = addrInput
            , port = portInput
            , pacPath = pacPathInput
            , pacBody = pacBodyDisp
            , stop = butStop
            }

    setCallback butStart (startCallback statusAppend pf pa)

    end buttonsPanel

    end gr
    hide gr
    return gr

proxyCreateInput :: Text -> IO ProxyResult
proxyCreateInput label = uiCreateTextDisplayGroup label "Proxy Server Input Data"

proxyCreateForwarded :: Text -> IO ProxyResult
proxyCreateForwarded label = uiCreateTextDisplayGroup label "Proxy Server Forwarded Data"

proxyCreateOutput :: Text -> IO ProxyResult
proxyCreateOutput label = uiCreateTextDisplayGroup label "Proxy Server Output Data"
