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

module UI.Tree
    ( treeCreate
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import Actions
import UI.About
import UI.Common
import UI.Destination
import UI.Proxy
import UI.Status

type TreeResult = (Ref Tree, ActionsUI)

showGroup :: Vector (Ref Group) -> Text -> IO ()
showGroup groups name = do
    forM_ groups $ \gr -> do
        label <- getLabel gr
        if name == label then
            showWidget gr
        else
            hide gr

treeCallback :: (Text -> IO ()) -> Vector (Ref Group) -> Ref Tree -> IO ()
treeCallback statusAppend groups tree = do
    (Just item) <- getCallbackItem tree
    label <- getLabel item
    (Just parent) <- getParent item
    parentLabel <- getLabel parent
    let path = if "ROOT" == parentLabel then label else parentLabel <> "/" <> label
    reason <- getCallbackReason tree
    when (TreeReasonSelected == reason) $ do
        statusAppend path
        showGroup groups path
    return ()

treeCreate :: ActionsBackground -> Ref TextDisplay -> IO TreeResult
treeCreate _ab statusDisp = do
    let CommonRectangles {treeRect} = commonRectangles
    let statusAppend = statusMessage statusDisp

    tree <- treeNew treeRect Nothing
    setShowroot tree False
    end tree

    let proxyRootLabel = "Proxy"
    _ <- add tree proxyRootLabel
    proxyRootGroup <- proxyCreateRoot proxyRootLabel statusAppend

    let proxyInputLabel = "Proxy/Input"
    _ <- add tree proxyInputLabel
    (proxyInputGroup, proxyInputAppend) <- proxyCreateInput proxyInputLabel

    let proxyForwardedLabel = "Proxy/Forwarded"
    _ <- add tree proxyForwardedLabel
    (proxyForwardedGroup, proxyForwardedAppend) <- proxyCreateForwarded proxyForwardedLabel

    let proxyReceivedLabel = "Proxy/Received"
    _ <- add tree proxyReceivedLabel
    (proxyReceivedGroup, proxyReceivedAppend) <- proxyCreateReceived proxyReceivedLabel

    let proxyOutputLabel = "Proxy/Output"
    _ <- add tree proxyOutputLabel
    (proxyOutputGroup, proxyOutputAppend) <- proxyCreateOutput proxyOutputLabel

    let destRootLabel = "Destination"
    _ <- add tree destRootLabel
    destRootGroup <- destinationCreateRoot destRootLabel statusAppend

    let destInputLabel = "Destination/Input"
    _ <- add tree destInputLabel
    (destInputGroup, destInputAppend) <- destinationCreateInput destInputLabel

    let destOutputLabel = "Destination/Output"
    _ <- add tree destOutputLabel
    (destOutputGroup, destOutputAppend) <- destinationCreateOutput destOutputLabel

    let aboutLabel = "About"
    _ <- add tree aboutLabel
    aboutGroup <- aboutCreate aboutLabel

    let groups = fromList
            [ proxyRootGroup
            , proxyInputGroup
            , proxyForwardedGroup
            , proxyReceivedGroup
            , proxyOutputGroup
            , destRootGroup
            , destInputGroup
            , destOutputGroup
            , aboutGroup
            ]

    setCallback tree (treeCallback statusAppend groups)

    showGroup groups "About"

    let actions = ActionsUI
            { statusAppend = statusAppend
            , showContentGroup = showGroup groups
            , proxyInputAppend = proxyInputAppend
            , proxyForwardedAppend = proxyForwardedAppend
            , proxyReceivedAppend = proxyReceivedAppend
            , proxyOutputAppend = proxyOutputAppend
            , destInputAppend = destInputAppend
            , destOutputAppend = destOutputAppend
            }

    return (tree, actions)
