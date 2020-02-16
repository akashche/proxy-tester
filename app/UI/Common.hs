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
    , CommonContext(..)
    , commonConstants
    , commonRectangles
    , commonCreateHeader
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
    , debugHeight :: Int
    } deriving (Generic, Show)

commonConstants :: CommonConstants
commonConstants = CommonConstants
    { borderSize = 10
    , headerHeight = 40
    , windowWidth = 640
    , windowMinWidth = 320
    , windowHeight = 480
    , windowMinHeight = 200
    , treeWidth = 200
    , debugHeight = 0 -- 200
    }

data CommonRectangles = CommonRectangles
    { tileRect :: Rectangle
    , treeRect :: Rectangle
    , contentRect :: Rectangle
    , contentBodyRect :: Rectangle
    , debugRect :: Rectangle
    } deriving (Generic, Show)

commonRectangles :: CommonRectangles
commonRectangles =
    let
        CommonConstants
                { windowWidth = ww
                , windowHeight = wh
                , debugHeight = dh
                , treeWidth = tw
                , headerHeight = hh
                , borderSize = bs
                } = commonConstants
    in
        CommonRectangles
                { tileRect = toRectangle (0, 0, ww, wh - dh)
                , treeRect = toRectangle (0, 0, tw, wh - dh)
                , contentRect = toRectangle (tw, 0, ww - tw , wh - dh)
                , contentBodyRect = toRectangle (tw, bs + hh, ww - tw, wh - dh - hh - bs)
                , debugRect = toRectangle (0, wh - dh, ww, dh)
                }

data CommonContext = CommonContext
    { showContentGroup :: Text -> IO ()
    , proxyInputAppend :: Text -> IO ()
    }

commonCreateHeader :: Text -> IO (Ref Box)
commonCreateHeader label = do
    let CommonConstants
            { borderSize = b
            , headerHeight = hh
            } = commonConstants
    let CommonRectangles {contentRect} = commonRectangles
    let (x, y, w, _) = fromRectangle contentRect
    let rect = toRectangle (x + b, y + b, w - (b*2), hh)
    box <- boxNew rect (Just label)
    setAlign box (Alignments
            [ AlignTypeCenter
            , AlignTypeInside
            , AlignTypeWrap
            ])
    setLabelsize box (FontSize 18)
    return box
