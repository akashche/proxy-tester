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

module DestinationServer
    ( destinationServerStart
    , destinationServerStop
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai.Handler.Warp as Warp

destinationServerStart :: (Text -> IO ()) -> IO (MVar.MVar ())
destinationServerStart statusAppend = do
    handle <- MVar.newEmptyMVar
    let settings = Warp.setPort 8080 Warp.defaultSettings
    _ <- Concurrent.forkIO $ Async.race_ (MVar.takeMVar handle) $ Warp.runSettings settings $ \_ respond -> do
        respond $ responseLBS HTTPTypes.status200 [] $
            "hello from server"
    statusAppend "Server started"
    return handle

destinationServerStop :: (Text -> IO ()) -> (MVar.MVar ()) -> IO ()
destinationServerStop statusAppend handle = do
    MVar.putMVar handle ()
    statusAppend "Server stopped"
