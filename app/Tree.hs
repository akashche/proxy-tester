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

module Tree
    ( treeCreate
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import Actions
import About
import UICommon
import Destination
import Proxy
import Status

type TreeResult = (Ref Tree, Actions)

showGroup :: Vector (Ref Group) -> Text -> IO ()
showGroup groups name = do
    forM_ groups $ \gr -> do
        label <- getLabel gr
        if name == label then
            showWidget gr
        else
            hide gr

treeCallback :: (Text -> IO ()) -> Vector (Ref Group) -> Ref Tree -> IO ()
treeCallback _status groups tree = do
    (Just item) <- getCallbackItem tree
    label <- getLabel item
    (Just parent) <- getParent item
    parentLabel <- getLabel parent
    let path = if "ROOT" == parentLabel then label else parentLabel <> "/" <> label
    reason <- getCallbackReason tree
    when (TreeReasonSelected == reason) $ do
--         status path
        showGroup groups path
    return ()

treeCreate :: Ref TextDisplay -> IO TreeResult
treeCreate statusDisp = do
    let UIRectangles {treeRect} = uiRectangles
    let statusAppend = statusMessage statusDisp

    tree <- treeNew treeRect Nothing
    setShowroot tree False
    end tree

    -- proxy
    let proxyRootLabel = "Proxy"
    let proxyInputLabel = "Proxy/Input"
    let proxyForwardedLabel = "Proxy/Forwarded"
    let proxyOutputLabel = "Proxy/Output"

    _ <- add tree proxyRootLabel
    _ <- add tree proxyInputLabel
    (proxyInputGroup, proxyInputAppend, proxyInputClear) <- proxyCreateInput proxyInputLabel
    _ <- add tree proxyForwardedLabel
    (proxyForwardedGroup, proxyForwardedAppend, proxyForwardedClear) <- proxyCreateForwarded proxyForwardedLabel
    _ <- add tree proxyOutputLabel
    (proxyOutputGroup, proxyOutputAppend, proxyOutputClear) <- proxyCreateOutput proxyOutputLabel
    let pa = ProxyAppenders
            { input = proxyInputAppend
            , inputClear = proxyInputClear
            , forwarded = proxyForwardedAppend
            , forwardedClear = proxyForwardedClear
            , output = proxyOutputAppend
            , outputClear = proxyOutputClear
            }
    proxyRootGroup <- proxyCreateRoot proxyRootLabel statusAppend pa

    -- destination
    let destRootLabel = "Destination"
    let destInputLabel = "Destination/Input"
    let destOutputLabel = "Destination/Output"
    _ <- add tree destRootLabel
    _ <- add tree destInputLabel
    (destInputGroup, destInputAppend, destInputClear) <- destinationCreateInput destInputLabel
    _ <- add tree destOutputLabel
    (destOutputGroup, destOutputAppend, destOutputClear) <- destinationCreateOutput destOutputLabel
    let da = DestinationAppenders
            { input = destInputAppend
            , inputClear = destInputClear
            , output = destOutputAppend
            , outputClear = destOutputClear
            }
    destRootGroup <- destinationCreateRoot destRootLabel statusAppend da

    let aboutLabel = "About"
    _ <- add tree aboutLabel
    aboutGroup <- aboutCreate aboutLabel

    let groups = fromList
            [ proxyRootGroup
            , proxyInputGroup
            , proxyForwardedGroup
            , proxyOutputGroup
            , destRootGroup
            , destInputGroup
            , destOutputGroup
            , aboutGroup
            ]

    setCallback tree (treeCallback statusAppend groups)

    showGroup groups "About"

    let actions = Actions
            { statusAppend = statusAppend
            , showContentGroup = showGroup groups
            , proxyInputAppend = proxyInputAppend
            , proxyForwardedAppend = proxyForwardedAppend
            , proxyOutputAppend = proxyOutputAppend
            , destInputAppend = destInputAppend
            , destOutputAppend = destOutputAppend
            }

    return (tree, actions)
