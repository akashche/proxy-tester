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

module UI.MainWindow
    ( mainWindowCreate
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import Actions
import UI.Common
import UI.Status
import UI.Tree

type MainWindow = (Ref DoubleWindow, ActionsUI)

mainWindowCreate :: IO MainWindow
mainWindowCreate = do
    let CommonConstants
            { windowWidth
            , windowMinWidth
            , windowHeight
            , windowMinHeight
            } = commonConstants
    let CommonRectangles
            { windowRect
            , tileRect
            } = commonRectangles
    let ws = toSize (windowWidth, windowHeight)

    window <- doubleWindowNew ws Nothing (Just "Proxy Tester")
    sizeRange window windowMinWidth windowMinHeight
    setResizable window (Nothing :: Maybe (Ref Group))
    mainTile <- tileNew windowRect Nothing
    statusDisp <- statusCreate
    tile <- tileNew tileRect Nothing
    (tree, actions) <- treeCreate statusDisp
    setResizable window (Just tree)
    end tile
    end mainTile
    end window
    return (window, actions)
