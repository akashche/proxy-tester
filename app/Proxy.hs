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
    ( proxyCreateRoot
    , proxyCreateInput
    , proxyCreateForwarded
    , proxyCreateReceived
    , proxyCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import UICommon

type ProxyResult = (Ref Group, Text -> IO ())

startCallback ::(Text -> IO ()) -> Ref Input -> Ref IntInput -> Ref Button -> Ref Button -> IO ()
startCallback statusAppend addr port stop start = do
    av <- getValue addr
    pv <- getValue port
    statusAppend ("start called, address: " <> av <> ", port: " <> (textShow pv))
    deactivate start
    setCallback stop $ \_ -> do
        statusAppend "stop called"
        deactivate stop
        activate start
        return ()
    activate stop

proxyCreateRoot :: Text -> (Text -> IO ()) -> IO (Ref Group)
proxyCreateRoot label statusAppend = do
    let CommonConstants
            { borderSize = bs
            , formRowHeight = frh
            , formLabelWidth = flw
            , formInputWidth = fiw
            , buttonWidth = btw
            , buttonHeight = bth
            } = commonConstants
    let CommonRectangles
            { contentRect
            , contentBodyRect
            , formRect
            , buttonsPanelRect
            } = commonRectangles
    let (bx, by, _, _) = fromRectangle contentBodyRect

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- commonCreateHeader "Proxy Server"

    body <- groupNew formRect Nothing
    setResizable gr (Just body)
    form <- groupNew formRect Nothing
    setResizable form (Nothing :: Maybe (Ref Box))

    -- address
    addrLabel <- boxNew (toRectangle (bx, by + bs, flw, frh)) (Just "IP Address")
    commonSetLabelAlign addrLabel
    addrInput <- inputNew (toRectangle (bx + flw + bs, by + bs, fiw, frh)) Nothing Nothing
    _ <- setValue addrInput "127.0.0.1" Nothing

    -- port
    portLabel <- boxNew (toRectangle (bx, by + bs*4, flw, frh)) (Just "TCP Port")
    commonSetLabelAlign portLabel
    portInput <- intInputNew (toRectangle (bx + flw + bs, by + bs*4, fiw, frh)) Nothing
    _ <- setValue portInput "8082" Nothing

    -- dest address
    destAddrLabel <- boxNew (toRectangle (bx, by + bs*7, flw, frh)) (Just "Dest IP Address")
    commonSetLabelAlign destAddrLabel
    destAddrInput <- inputNew (toRectangle (bx + flw + bs, by + bs*7, fiw, frh)) Nothing Nothing
    _ <- setValue destAddrInput "127.0.0.1" Nothing

    -- dest port
    destPortLabel <- boxNew (toRectangle (bx, by + bs*10, flw, frh)) (Just "Dest TCP Port")
    commonSetLabelAlign destPortLabel
    destPortInput <- intInputNew (toRectangle (bx + flw + bs, by + bs*10, fiw, frh)) Nothing
    _ <- setValue destPortInput "8081" Nothing

    -- invisible
    invLabel <- boxNew (toRectangle (bx, by + bs*13, flw, frh)) (Just "")
    commonSetLabelAlign invLabel
    invBox <- boxNew (toRectangle (bx + flw + bs, by + bs*13, fiw, frh)) Nothing
    setResizable form (Just invBox)
    hide invBox

    end form
    end body

    buttonsPanel <- groupNew buttonsPanelRect Nothing
    setBox buttonsPanel EngravedBox
    let (bpx, bpy, bpw, _) = fromRectangle buttonsPanelRect

    butStart <- buttonNew (toRectangle (bpx + bpw - bs*2 - btw*2, bpy + bs, btw, bth)) (Just "Start")
    butStop <- buttonNew (toRectangle (bpx + bpw - bs - btw, bpy + bs, btw, bth)) (Just "Stop")
    deactivate butStop
    let cb = startCallback statusAppend addrInput portInput butStop
    setCallback butStart cb

    end buttonsPanel

    end gr
    hide gr
    return gr

proxyCreateInput :: Text -> IO ProxyResult
proxyCreateInput label = commonCreateTextDisplayGroup label "Proxy Server Input Data"

proxyCreateForwarded :: Text -> IO ProxyResult
proxyCreateForwarded label = commonCreateTextDisplayGroup label "Proxy Server Forwarded Data"

proxyCreateReceived :: Text -> IO ProxyResult
proxyCreateReceived label = commonCreateTextDisplayGroup label "Proxy Server Received Data"

proxyCreateOutput :: Text -> IO ProxyResult
proxyCreateOutput label = commonCreateTextDisplayGroup label "Proxy Server Output Data"
