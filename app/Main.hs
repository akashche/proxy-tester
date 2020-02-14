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
{-# LANGUAGE Strict #-}

module Main where

import Prelude ()
import VtUtils.Prelude
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations (Boxtype(DownBox))
import Graphics.UI.FLTK.LowLevel.FLTKHS
        ( Group, Ref, Tree
        , add, begin, doubleWindowNew, getH, getW, groupNew, end
        , setBox, setCallback, setResizable, setShowroot, showWidget, sizeRange
        , tileNew, treeNew, toRectangle, toSize
        )

treeElements:: [Text]
treeElements =
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

treeCallback :: Ref Tree -> IO ()
treeCallback _tree' = do
    return ()
--  (Just item') <- getCallbackItem tree'
--  reason' <- getCallbackReason tree'
--  label' <- pack <$> getLabel item'
--  case reason' of
--    TreeReasonSelected -> do
--      (Just path') <- itemPathname tree' item'
--      putStrLn $ "TreeCallback: Item selected =" <> label' <> "Full pathname=" <> (unpack path') <> "\n"
--    TreeReasonDeselected ->
--      print $ "TreeCallback: Item deselected =" ++ (unpack label') ++ "\n"
--    TreeReasonOpened ->
--      print $ "TreeCallback: Item opened1 =" ++ (unpack label') ++ "\n"
--    TreeReasonClosed ->
--      print $ "TreeCallback: Item closed =" ++ (unpack label') ++ "\n"
--    _ -> print ("" :: String)

main :: IO ()
main = do
    window <- doubleWindowNew (toSize (640, 480)) Nothing (Just "Simple Tree")
    sizeRange window 1 1
    setResizable window (Just window)
    windowWidth <- getW window
    windowHeight <- getH window
    begin window

    tile <- tileNew (toRectangle (0, 0, windowWidth, windowHeight)) Nothing
    begin tile

    tree <- treeNew (toRectangle (0, 0, 200, windowHeight)) Nothing
    setShowroot tree False
    forM_ (treeElements) $ \el -> do
      _ <- add tree el
      return ()
    setCallback tree treeCallback

    rightGroup <- groupNew (toRectangle (200, 0, windowWidth - 200, windowHeight)) Nothing
    setBox rightGroup DownBox
    rightGroupWidth <- getW rightGroup
    setResizable rightGroup (Nothing :: Maybe (Ref Group))
    begin rightGroup

    contentGroup <- groupNew (toRectangle (200, 0, rightGroupWidth, windowHeight - 40)) Nothing
    setBox contentGroup DownBox
    setResizable rightGroup (Just contentGroup)

    buttonsGroup <- groupNew (toRectangle (200, windowHeight - 40, rightGroupWidth, 40)) Nothing
    setBox buttonsGroup DownBox

    end rightGroup
    end tile
    end window
    showWidget window
    _ <- FL.run
    return ()

