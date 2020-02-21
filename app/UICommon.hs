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

module UICommon
    ( UIConstants(..)
    , UIRectangles(..)
    , uiConstants
    , uiRectangles
    , uiCreateHeader
    , uiCreateTextDisplayGroup
    , uiTextDisplayAppend
    , uiTextDisplayClear
    , uiTextDisplayMessage
    , uiSetLabelAlign
    , uiGetTextDisplayValue
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

data UIConstants = UIConstants
    { borderSize :: Int
    , headerHeight :: Int
    , windowWidth :: Int
    , windowMinWidth :: Int
    , windowHeight :: Int
    , windowMinHeight :: Int
    , treeWidth :: Int
    , statusHeight :: Int
    , formRowHeight :: Int
    , formLabelWidth :: Int
    , formInputWidth :: Int
    , buttonsPanelHeight :: Int
    , buttonWidth :: Int
    , buttonHeight :: Int
    } deriving (Show)

uiConstants :: UIConstants
uiConstants = UIConstants
    { borderSize = 10
    , headerHeight = 40
    , windowWidth = 640
    , windowMinWidth = 320
    , windowHeight = 480
    , windowMinHeight = 200
    , treeWidth = 200
    , statusHeight = 100
    , formRowHeight = 20
    , formLabelWidth = 150
    , formInputWidth = 120
    , buttonsPanelHeight = 50
    , buttonWidth = 100
    , buttonHeight = 30
    }

data UIRectangles = UIRectangles
    { windowRect :: Rectangle
    , tileRect :: Rectangle
    , treeRect :: Rectangle
    , contentRect :: Rectangle
    , contentBodyRect :: Rectangle
    , formRect :: Rectangle
    , buttonsPanelRect :: Rectangle
    , statusRect :: Rectangle
    } deriving (Show)

uiRectangles :: UIRectangles
uiRectangles =
    let
        UIConstants
                { windowWidth = ww
                , windowHeight = wh
                , statusHeight = sh
                , treeWidth = tw
                , headerHeight = hh
                , buttonsPanelHeight = bph
                } = uiConstants
    in
        UIRectangles
                { windowRect = toRectangle (0, 0, ww, wh)
                , tileRect = toRectangle (0, 0, ww, wh - sh)
                , treeRect = toRectangle (0, 0, tw, wh - sh)
                , contentRect = toRectangle (tw, 0, ww - tw, wh - sh)
                , contentBodyRect = toRectangle (tw, hh, ww - tw, wh - sh - hh)
                , formRect = toRectangle (tw, hh, ww - tw, wh - sh - hh - bph)
                , buttonsPanelRect = toRectangle (tw, wh - sh - bph, ww - tw, bph)
                , statusRect = toRectangle (0, wh - sh, ww, sh)
                }

uiCreateHeader :: Text -> IO (Ref Box)
uiCreateHeader label = do
    let UIConstants
            { borderSize = bs
            , headerHeight = hh
            } = uiConstants
    let UIRectangles {contentRect} = uiRectangles
    let (x, y, w, _) = fromRectangle contentRect
    let rect = toRectangle (x + bs, y, w - (bs*2), hh)
    box <- boxNew rect (Just label)
    setAlign box (Alignments
            [ AlignTypeCenter
            , AlignTypeInside
            , AlignTypeWrap
            ])
    setLabelsize box (FontSize 18)
    return box

uiCreateTextDisplayGroup :: Text -> Text -> IO (Ref Group, Text -> IO (), IO ())
uiCreateTextDisplayGroup label header = do
    let UIRectangles
            { contentRect
            , contentBodyRect
            } = uiRectangles

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- uiCreateHeader header

    disp <- textDisplayNew contentBodyRect Nothing
    setTextsize disp (FontSize 12)
    buf <- textBufferNew Nothing Nothing
    setBuffer disp (Just buf)
    end disp

    let append = uiTextDisplayMessage disp
    let clear = uiTextDisplayClear disp

    setResizable gr (Just disp)
    end gr
    hide gr
    return (gr, append, clear)

uiTextDisplayAppend :: Ref TextDisplay -> Text -> IO ()
uiTextDisplayAppend disp msg = do
    mbuf <- getBuffer disp
    case mbuf of
        Just buf -> do
            appendToBuffer buf msg
            appendToBuffer buf "\n"
        Nothing -> return ()

uiTextDisplayClear :: Ref TextDisplay -> IO ()
uiTextDisplayClear disp = do
    mbuf <- getBuffer disp
    case mbuf of
        Just buf -> do
            setText buf ""
        Nothing -> return ()

uiTextDisplayMessage :: Ref TextDisplay -> Text -> IO ()
uiTextDisplayMessage disp msg = do
    _ <- fltkhsLock
    date <- (dateFormat "[%H:%M:%S]") <$> getCurrentTime
    uiTextDisplayAppend disp (date <> " " <> msg)
    _ <- fltkhsUnlock
    _ <- fltkhsAwake
    return ()

uiSetLabelAlign :: Ref Box -> IO ()
uiSetLabelAlign box = do
    _ <- setAlign box (Alignments
            [ AlignTypeRight
            , AlignTypeCenter
            , AlignTypeInside
            , AlignTypeWrap
            ])
    return ()

uiGetTextDisplayValue :: Ref TextDisplay -> IO Text
uiGetTextDisplayValue disp = do
    mbuf <- getBuffer disp
    case mbuf of
        Just buf -> getText buf
        Nothing -> return ""
