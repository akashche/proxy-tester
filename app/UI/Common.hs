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

module UI.Common
    ( CommonConstants(..)
    , CommonRectangles(..)
    , commonConstants
    , commonRectangles
    , commonCreateHeader
    , commonCreateTextDisplayGroup
    , commonTextDisplayMessage
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

data CommonConstants = CommonConstants
    { borderSize :: Int
    , headerHeight :: Int
    , windowWidth :: Int
    , windowMinWidth :: Int
    , windowHeight :: Int
    , windowMinHeight :: Int
    , treeWidth :: Int
    , statusHeight :: Int
    } deriving (Show)

commonConstants :: CommonConstants
commonConstants = CommonConstants
    { borderSize = 10
    , headerHeight = 40
    , windowWidth = 640
    , windowMinWidth = 320
    , windowHeight = 480
    , windowMinHeight = 200
    , treeWidth = 200
    , statusHeight = 100
    }

data CommonRectangles = CommonRectangles
    { windowRect :: Rectangle
    , tileRect :: Rectangle
    , treeRect :: Rectangle
    , contentRect :: Rectangle
    , contentBodyRect :: Rectangle
    , statusRect :: Rectangle
    } deriving (Show)

commonRectangles :: CommonRectangles
commonRectangles =
    let
        CommonConstants
                { windowWidth = ww
                , windowHeight = wh
                , statusHeight = sh
                , treeWidth = tw
                , headerHeight = hh
                } = commonConstants
    in
        CommonRectangles
                { windowRect = toRectangle (0, 0, ww, wh)
                , tileRect = toRectangle (0, 0, ww, wh - sh)
                , treeRect = toRectangle (0, 0, tw, wh - sh)
                , contentRect = toRectangle (tw, 0, ww - tw, wh - sh)
                , contentBodyRect = toRectangle (tw, hh, ww - tw, wh - sh - hh)
                , statusRect = toRectangle (0, wh - sh, ww, sh)
                }

commonCreateHeader :: Text -> IO (Ref Box)
commonCreateHeader label = do
    let CommonConstants
            { borderSize = bs
            , headerHeight = hh
            } = commonConstants
    let CommonRectangles {contentRect} = commonRectangles
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

commonCreateTextDisplayGroup :: Text -> Text -> IO (Ref Group, Text -> IO ())
commonCreateTextDisplayGroup label header = do
    let CommonRectangles
            { contentRect
            , contentBodyRect
            } = commonRectangles

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- commonCreateHeader header

    disp <- textDisplayNew contentBodyRect Nothing
    setTextsize disp (FontSize 12)
    buf <- textBufferNew Nothing Nothing
    setBuffer disp (Just buf)
    end disp

    let append = commonTextDisplayMessage disp

    setResizable gr (Just disp)
    end gr
    hide gr
    return (gr, append)

commonTextDisplayMessage :: Ref TextDisplay -> Text -> IO ()
commonTextDisplayMessage disp msg = do
    mbuf <- getBuffer disp
    case mbuf of
        Just buf -> do
            appendToBuffer buf msg
            appendToBuffer buf "\n"
        Nothing -> return ()
