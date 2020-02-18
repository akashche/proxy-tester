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

module Main where

import Prelude ()
import VtUtils.Prelude
import FLTKHSPrelude

import Actions
import UI.MainWindow

main :: IO ()
main = do
    mw <- mainWindowCreate
    let (window, actions) = mw
    showWidget window
    let ActionsUI {showContentGroup} = actions

    _ <- fltkhsLock

    showContentGroup "About"

--     _ <- Concurrent.forkOS $ do
--         forM_ (Vector.replicate 5 (0 :: Int)) $ \_ -> do
--             Concurrent.threadDelay 3000000
--             _ <- fltkhsLock
--             proxyInputAppend "Hello"
--             _ <- fltkhsUnlock
--             _ <- fltkhsAwake
--             return ()

    _ <- fltkhsRun
    return ()
