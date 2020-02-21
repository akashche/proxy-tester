--
-- Copyright 2018, akashche at redhat.com
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

module FLTKHSPrelude
    ( Alignments(..), AlignType(..), Boxtype(..)
    , Box, Button, DoubleWindow, FontSize(..), Group, Input, IntInput, Rectangle
    , Ref, Size, TextBuffer, TextDisplay, TextEditor, Tree, TreeReasonType(..)
    , activate, add, appendToBuffer
    , buttonNew, boxNew, boxNewWithBoxtype
    , deactivate, doubleWindowNew
    , fltkhsAwake, fltkhsLock, fltkhsRun, fltkhsUnlock, fromRectangle
    , getBuffer, getCallbackItem, getLabel, getCallbackReason, getParent, getText, getValue
    , groupNew, hide, end
    , inputNew, intInputNew
    , setAlign, setBox, setBuffer, setCallback, setLabelsize, setResizable
    , setShowroot, setText, setTextsize, setValue
    , showWidget, sizeRange
    , textBufferNew, textDisplayNew, textEditorNew, tileNew, treeNew, toRectangle, toSize
    ) where

import Prelude (IO, Int, Bool)
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations (Alignments(..), AlignType(..), Boxtype(..))
import Graphics.UI.FLTK.LowLevel.FLTKHS
        ( Box, Button, DoubleWindow, FontSize(..), Group, Input, IntInput, Rectangle
        , Ref, Size, TextBuffer, TextDisplay, TextEditor, Tree, TreeReasonType(..)
        , activate, add, appendToBuffer
        , buttonNew, boxNew, boxNewWithBoxtype
        , deactivate, doubleWindowNew
        , fromRectangle
        , getBuffer, getCallbackItem, getLabel, getCallbackReason, getParent, getText, getValue
        , groupNew, hide, end
        , inputNew, intInputNew
        , setAlign, setBox, setBuffer, setCallback, setLabelsize, setResizable
        , setShowroot, setText, setTextsize, setValue
        , showWidget, sizeRange
        , textBufferNew, textDisplayNew, textEditorNew, tileNew, treeNew, toRectangle, toSize
        )


fltkhsRun :: IO Int
fltkhsRun = FL.run

fltkhsLock :: IO Bool
fltkhsLock = FL.lock

fltkhsUnlock :: IO ()
fltkhsUnlock = FL.unlock

fltkhsAwake :: IO ()
fltkhsAwake = FL.awake
