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

module UI.Destination
    ( destinationCreateRoot
    , destinationCreateInput
    , destinationCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import Actions
import UI.Common

type DestResult = (Ref Group, Text -> IO ())

startCallback ::(Text -> IO ()) -> ActionsBackground -> Ref Input -> Ref IntInput -> Ref Button -> Ref Button -> IO ()
startCallback statusAppend actions addr port stop start = do
    let ActionsBackground
            { destServerStart
            , destServerStop
            } = actions
    av <- getValue addr
    pv <- getValue port
    statusAppend ("start called, address: " <> av <> ", port: " <> (textShow pv))
    deactivate start
    server <- destServerStart
    setCallback stop $ \_ -> do
        statusAppend "stop called"
        deactivate stop
        destServerStop server
        activate start
        return ()
    activate stop

destinationCreateRoot :: Text -> (Text -> IO ()) -> ActionsBackground -> IO (Ref Group)
destinationCreateRoot label statusAppend actions = do
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
    _ <- commonCreateHeader "Destination Server"

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
    _ <- setValue portInput "8081" Nothing

    -- response
    respLabel <- boxNew (toRectangle (bx, by + bs*7, flw, frh)) (Just "Response Text")
    commonSetLabelAlign respLabel
    respDisp <- textDisplayNew (toRectangle (bx + flw + bs, by + bs*7, fiw*2, frh*8))Nothing
    setTextsize respDisp (FontSize 12)
    respBuf <- textBufferNew Nothing Nothing
    setBuffer respDisp (Just respBuf)
    commonTextDisplayMessage respDisp "Hello from destination server"
    end respDisp
    setResizable form (Just respDisp)

    end form
    end body

    buttonsPanel <- groupNew buttonsPanelRect Nothing
    setBox buttonsPanel EngravedBox
    let (bpx, bpy, bpw, _) = fromRectangle buttonsPanelRect

    butStart <- buttonNew (toRectangle (bpx + bpw - bs*2 - btw*2, bpy + bs, btw, bth)) (Just "Start")
    butStop <- buttonNew (toRectangle (bpx + bpw - bs - btw, bpy + bs, btw, bth)) (Just "Stop")
    deactivate butStop
    let cb = startCallback statusAppend actions addrInput portInput butStop
    setCallback butStart cb

    end buttonsPanel

    end gr
    hide gr
    return gr

destinationCreateInput :: Text -> IO DestResult
destinationCreateInput label = commonCreateTextDisplayGroup label "Destination Server Input"

destinationCreateOutput :: Text -> IO DestResult
destinationCreateOutput label = commonCreateTextDisplayGroup label "Destination Server Output"
