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

module UI.Proxy
    ( proxyCreateRoot
    , proxyCreateInput
    , proxyCreateForwarded
    , proxyCreateReceived
    , proxyCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import UI.Common

type ProxyResult = (Ref Group, Text -> IO ())

proxyCreateRoot :: Text -> IO (Ref Group)
proxyCreateRoot label = do
    let CommonRectangles
            { contentRect
            , contentBodyRect
            } = commonRectangles

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- commonCreateHeader "Proxy Server"
    body <- boxNew contentBodyRect (Just
            "[TODO]")
    setAlign body (Alignments
            [ AlignTypeCenter
            , AlignTypeTop
            , AlignTypeInside
            , AlignTypeWrap
            ])
    setResizable gr (Just body)
    end gr
    hide gr
    return gr

proxyCreateInput :: Text -> IO ProxyResult
proxyCreateInput label = commonCreateTextDisplayGroup label "Proxy Server Input Data"

proxyCreateForwarded :: Text -> IO ProxyResult
proxyCreateForwarded label = commonCreateTextDisplayGroup label "Proxy Server Forwarded Data"

proxyCreateReceived :: Text -> IO ProxyResult
proxyCreateReceived label = commonCreateTextDisplayGroup label "Proxy Server Received Data"

proxyCreateOutput :: Text -> IO ProxyResult
proxyCreateOutput label = commonCreateTextDisplayGroup label "Proxy Server Output Data"
