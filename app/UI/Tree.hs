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

import UI.About
import UI.Common
import UI.Debug
-- import UI.Destination
import UI.Proxy

-- treeElements :: Vector (Text, Text -> IO (Ref Group))
-- treeElements = fromList
--     [ ("Destination", destinationCreateRoot)
--     , ("Destination/Server", destinationCreateServer)
--     , ("Destination/Input", destinationCreateInput)
--     , ("Destination/Output", destinationCreateOutput)
--     , ("Proxy", proxyCreateRoot)
--     , ("Proxy/Server", proxyCreateServer)
--     , ("Proxy/Input", proxyCreateInput)
--     , ("Proxy/Forwarded", proxyCreateForwarded)
--     , ("Proxy/Received", proxyCreateReceived)
--     , ("Proxy/Output", proxyCreateOutput)
--     , ("About", aboutCreate)
--     ]

showGroup :: Vector (Ref Group) -> Text -> IO ()
showGroup groups name = do
    forM_ groups $ \gr -> do
        label <- getLabel gr
        if name == label then
            showWidget gr
        else
            hide gr

treeCallback :: (Text -> IO ()) -> Vector (Ref Group) -> Ref Tree -> IO ()
treeCallback debug groups tree = do
    (Just item) <- getCallbackItem tree
    label <- getLabel item
    (Just parent) <- getParent item
    parentLabel <- getLabel parent
    let path = if "ROOT" == parentLabel then label else parentLabel <> "/" <> label
    reason <- getCallbackReason tree
    when (TreeReasonSelected == reason) $ do
        debug path
        showGroup groups path
    return ()

treeCreate :: Ref TextDisplay -> IO (CommonContext, (Ref Tree))
treeCreate debugDisp = do
    let CommonRectangles {treeRect} = commonRectangles

    tree <- treeNew treeRect Nothing
    setShowroot tree False
    end tree

    let proxyRootName = "Proxy"
    _ <- add tree proxyRootName
    proxyRootGroup <- proxyCreateRoot proxyRootName

    let proxyInputName = "Proxy/Input"
    _ <- add tree proxyInputName
    (proxyInputAppend, proxyInputGroup) <- proxyCreateInput proxyInputName

    let aboutName = "About"
    _ <- add tree aboutName
    aboutGroup <- aboutCreate aboutName

    let groups = fromList
            [ proxyRootGroup
            , proxyInputGroup
            , aboutGroup
            ]

    let debug = debugMessage debugDisp
    setCallback tree (treeCallback debug groups)

    showGroup groups "About"

    let ctx = CommonContext
            { showContentGroup = showGroup groups
            , proxyInputAppend = proxyInputAppend
            }

    return (ctx, tree)
