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
    , proxyCreateServer
    , proxyCreateInput
    , proxyCreateForwarded
    , proxyCreateReceived
    , proxyCreateOutput
    ) where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import UI.Common

createPlaceholder :: Text -> Text -> IO (Ref Group)
createPlaceholder label header = do
    let CommonRectangles
            { contentRect
            , contentBodyRect
            } = commonRectangles

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- commonCreateHeader header
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

proxyCreateRoot :: Text -> IO (Ref Group)
proxyCreateRoot label = createPlaceholder label "Proxy"

proxyCreateServer :: Text -> IO (Ref Group)
proxyCreateServer label = createPlaceholder label "Proxy Server"

inputAppend :: (Ref TextBuffer) -> Text -> IO ()
inputAppend buf msg = do
    appendToBuffer buf msg
    appendToBuffer buf "\n"


proxyCreateInput :: Text -> IO (Text -> IO (), (Ref Group))
proxyCreateInput label = do
    let CommonRectangles
            { contentRect
            , contentBodyRect
            } = commonRectangles

    gr <- groupNew contentRect (Just label)
    setBox gr DownBox
    setResizable gr (Nothing :: Maybe (Ref Box))
    _ <- commonCreateHeader "Proxy Server Input Data"

    disp <- textDisplayNew contentBodyRect Nothing
    buf <- textBufferNew Nothing Nothing
    setBuffer disp (Just buf)

    setResizable gr (Just disp)
    end gr
    hide gr

    return ((inputAppend buf), gr)

proxyCreateForwarded :: Text -> IO (Ref Group)
proxyCreateForwarded label = createPlaceholder label "Proxy Server Forwarded Data"

proxyCreateReceived :: Text -> IO (Ref Group)
proxyCreateReceived label = createPlaceholder label "Proxy Server Received Data"

proxyCreateOutput :: Text -> IO (Ref Group)
proxyCreateOutput label = createPlaceholder label "Proxy Server Output Data"
