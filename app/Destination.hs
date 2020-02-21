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

module Destination
    ( DestinationAppenders(..)
    , DestinationDisplayResult
    , destinationCreateRoot
    , destinationCreateInput
    , destinationCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import UICommon
import DestinationServer

data DestForm = DestForm
    { addr :: Ref Input
    , port :: Ref IntInput
    , resp :: Ref TextDisplay
    , stop :: Ref Button
    }

data DestinationAppenders = DestinationAppenders
    { input :: Text -> IO ()
    , inputClear :: IO ()
    , output :: Text -> IO ()
    , outputClear :: IO ()
    }

type DestinationDisplayResult = (Ref Group, Text -> IO (), IO ())

startCallback ::(Text -> IO ()) -> DestForm -> DestinationAppenders -> Ref Button -> IO ()
startCallback statusAppend form da start = do
    let DestForm {addr, port, resp, stop} = form
    let DestinationAppenders
            { input
            , inputClear
            , output
            , outputClear
            } = da
    av <- getValue addr
    pv <- (read . unpack) <$> getValue port :: IO Int
    rv <- uiGetTextDisplayValue resp
    deactivate start
    let dsa = DestinationServerOptions
            { input = input
            , output = output
            , status = statusAppend
            , host = av
            , port = pv
            , response = rv
            }
    server <- destinationServerStart dsa
    inputClear
    outputClear
    setCallback stop $ \_ -> do
        deactivate stop
        destinationServerStop server
        activate start
        return ()
    activate stop

destinationCreateRoot :: Text -> (Text -> IO ()) -> DestinationAppenders -> IO (Ref Group)
destinationCreateRoot label statusAppend da = do
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
    _ <- uiCreateHeader "Destination Server"

    body <- groupNew formRect Nothing
    setResizable gr (Just body)
    form <- groupNew formRect Nothing
    setResizable form (Nothing :: Maybe (Ref Box))

    -- address
    addrLabel <- boxNew (toRectangle (bx, by + bs, flw, frh)) (Just "IP Address")
    uiSetLabelAlign addrLabel
    addrInput <- inputNew (toRectangle (bx + flw + bs, by + bs, fiw, frh)) Nothing Nothing
    _ <- setValue addrInput "127.0.0.1" Nothing

    -- port
    portLabel <- boxNew (toRectangle (bx, by + bs*4, flw, frh)) (Just "TCP Port")
    uiSetLabelAlign portLabel
    portInput <- intInputNew (toRectangle (bx + flw + bs, by + bs*4, fiw, frh)) Nothing
    _ <- setValue portInput "8081" Nothing

    -- response
    respLabel <- boxNew (toRectangle (bx, by + bs*7, flw, frh)) (Just "Response Text")
    uiSetLabelAlign respLabel
    respDisp <- textDisplayNew (toRectangle (bx + flw + bs, by + bs*7, fiw*2, frh*8))Nothing
    setTextsize respDisp (FontSize 12)
    respBuf <- textBufferNew Nothing Nothing
    setBuffer respDisp (Just respBuf)
    uiTextDisplayAppend respDisp "Hello from destination server"
    end respDisp
    setResizable form (Just respDisp)

    end form
    end body

    buttonsPanel <- groupNew buttonsPanelRect Nothing
    setBox buttonsPanel EngravedBox
    let (bpx, bpy, bpw, _) = fromRectangle buttonsPanelRect

    startButt <- buttonNew (toRectangle (bpx + bpw - bs*2 - btw*2, bpy + bs, btw, bth)) (Just "Start")
    stopButt <- buttonNew (toRectangle (bpx + bpw - bs - btw, bpy + bs, btw, bth)) (Just "Stop")
    deactivate stopButt
    let df = DestForm
            { addr = addrInput
            , port = portInput
            , resp = respDisp
            , stop = stopButt
            }
    setCallback startButt (startCallback statusAppend df da)

    end buttonsPanel

    end gr
    hide gr
    return gr

destinationCreateInput :: Text -> IO DestinationDisplayResult
destinationCreateInput label = uiCreateTextDisplayGroup label "Destination Server Input"

destinationCreateOutput :: Text -> IO DestinationDisplayResult
destinationCreateOutput label = uiCreateTextDisplayGroup label "Destination Server Output"
