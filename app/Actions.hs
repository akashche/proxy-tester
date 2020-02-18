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

module Actions
    ( Actions(..)
    ) where

import Prelude ()
import VtUtils.Prelude

data Actions = Actions
    { statusAppend :: Text -> IO ()
    , showContentGroup :: Text -> IO ()
    , proxyInputAppend :: Text -> IO ()
    , proxyForwardedAppend :: Text -> IO ()
    , proxyReceivedAppend :: Text -> IO ()
    , proxyOutputAppend :: Text -> IO ()
    , destInputAppend :: Text -> IO ()
    , destOutputAppend :: Text -> IO ()
    , proxyInputAppend :: Text -> IO ()
    }


