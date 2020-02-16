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

module UI.Destination
    ( destinationCreateRoot
    , destinationCreateServer
    , destinationCreateInput
    , destinationCreateOutput
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

destinationCreateRoot :: Text -> IO (Ref Group)
destinationCreateRoot label = createPlaceholder label "Destination"

destinationCreateServer :: Text -> IO (Ref Group)
destinationCreateServer label = createPlaceholder label "Destination Server"

destinationCreateInput :: Text -> IO (Ref Group)
destinationCreateInput label = createPlaceholder label "Destination Server Input"

destinationCreateOutput :: Text -> IO (Ref Group)
destinationCreateOutput label = createPlaceholder label "Destination Server Output"
