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

module Main where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations (Boxtype(DownBox))
import Graphics.UI.FLTK.LowLevel.FLTKHS
        ( Group, Rectangle, Ref, Size, TextBuffer, Tree, TreeReasonType(TreeReasonSelected)
        , add, appendToBuffer, boxNew, doubleWindowNew, getCallbackItem, getLabel, getCallbackReason
        , getParent, groupNew, hide, end
        , setBox, setBuffer, setCallback, setResizable, setShowroot, showWidget, sizeRange
        , textDisplayNew, textBufferNew, tileNew, treeNew, toRectangle, toSize
        )

treeElements :: Vector Text
treeElements = fromList
    [ "Destination/Server"
    , "Destination/Input"
    , "Destination/Output"
    , "Proxy/Server"
    , "Proxy/Input"
    , "Proxy/Forwarded"
    , "Proxy/Received"
    , "Proxy/Output"
    , "About"
    ]

createGroup :: Rectangle -> Text -> IO (Ref Group)
createGroup rect name = do
        gr <- groupNew rect (Just name)
        setBox gr DownBox
        _ <- boxNew contentRect (Just name)
        end gr
        hide gr
        return gr

treeGroups :: Vector Text -> Rectangle -> IO (HashMap Text (Ref Group))
treeGroups names rect = do
    groups <- Vector.mapM fun names
    let pairs = Vector.zip names groups
    let map = HashMap.fromList (toList pairs)
    return map
    where
        fun nm = createGroup rect nm

treeCallback :: Ref TextBuffer -> HashMap Text (Ref Group) -> Ref Tree -> IO ()
treeCallback tbuff map tree = do
    (Just item) <- getCallbackItem tree
    label <- getLabel item
    (Just parent) <- getParent item
    parentLabel <- getLabel parent
    let path = if "ROOT" == parentLabel then label else parentLabel <> "/" <> label
    reason <- getCallbackReason tree
    when (TreeReasonSelected == reason) $ do
        forM_ (HashMap.elems map) $ \gr -> do
            hide gr
            return ()
        case lookup path map of
            Just gr -> do
                _ <- showWidget gr
                return ()
            Nothing -> return ()
        appendToBuffer tbuff path
        appendToBuffer tbuff "\n"
    return ()

windowWidth :: Int
windowWidth = 640

windowHeight :: Int
windowHeight = 480

treeWidth :: Int
treeWidth = 200

debugHeight :: Int
debugHeight = 200

windowSize :: Size
windowSize = toSize (windowWidth, windowHeight)

tileRect :: Rectangle
tileRect = toRectangle (0, 0, windowWidth, windowHeight - debugHeight)

treeRect :: Rectangle
treeRect = toRectangle (0, 0, treeWidth, windowHeight - debugHeight)

contentRect :: Rectangle
contentRect = toRectangle (treeWidth, 0, windowWidth - treeWidth, windowHeight - debugHeight)

debugRect :: Rectangle
debugRect = toRectangle (0, windowHeight - debugHeight, windowWidth, debugHeight)

main :: IO ()
main = do
    window <- doubleWindowNew windowSize Nothing (Just "Proxy Tester")
    sizeRange window 1 1
    setResizable window (Nothing :: Maybe (Ref Group))

    disp <- textDisplayNew debugRect Nothing
    tbuff <- textBufferNew Nothing Nothing
    appendToBuffer tbuff "Debug:\n"
    setBuffer disp (Just tbuff)
    end disp

    tile <- tileNew tileRect Nothing

    tree <- treeNew treeRect Nothing
    setShowroot tree False
    end tree
    setResizable window (Just tree)

    map <- treeGroups treeElements contentRect
    forM_ (treeElements) $ \el -> do
        _ <- add tree el
        return ()
    setCallback tree (treeCallback tbuff map)

    case lookup "About" map of
        Just gr -> do
            _ <- showWidget gr
            return ()
        Nothing -> return ()

    end tile
    end window

    showWidget window
    _ <- FL.run
    return ()

