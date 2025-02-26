{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Chainweb.Pact.TransactionExec
-- Copyright   :  Copyright © 2018 Kadena LLC.
-- License     :  (see the file LICENSE)
-- Maintainer  :  Emily Pillmore <emily@kadena.io>
-- Stability   :  experimental
--
-- The block validation logic for Pact Service
--
-- This exists due to moving things around resolving
-- chainweb dependencies. This should find a new home.
--
module Chainweb.Pact.Service.BlockValidation
( validateBlock
, newBlock
, local
, lookupPactTxs
) where


import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM.TQueue
import Data.Tuple.Strict
import Data.Vector (Vector)
import qualified Pact.Types.Hash as P

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Transaction


newBlock :: Miner -> BlockHeader -> TQueue RequestMsg ->
            IO (MVar (Either PactException PayloadWithOutputs))
newBlock mi bHeader reqQ = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = NewBlockMsg NewBlockReq
          { _newBlockHeader = bHeader
          , _newMiner = mi
          , _newResultVar = resultVar }
    addRequest reqQ msg
    return resultVar

validateBlock
    :: BlockHeader
    -> PayloadData
    -> TQueue RequestMsg
    -> IO (MVar (Either PactException PayloadWithOutputs))
validateBlock bHeader plData reqQ = do
    !resultVar <- newEmptyMVar :: IO (MVar (Either PactException PayloadWithOutputs))
    let !msg = ValidateBlockMsg ValidateBlockReq
          { _valBlockHeader = bHeader
          , _valResultVar = resultVar
          , _valPayloadData = plData }
    addRequest reqQ msg
    return resultVar

local :: ChainwebTransaction -> TQueue RequestMsg -> IO (MVar (Either PactException HashCommandResult))
local ct reqQ = do
    !resultVar <- newEmptyMVar
    let !msg = LocalMsg LocalReq
          { _localRequest = ct
          , _localResultVar = resultVar }
    addRequest reqQ msg
    return resultVar

lookupPactTxs
    :: T2 BlockHeight BlockHash
    -> Vector P.PactHash
    -> TQueue RequestMsg
    -> IO (MVar (Either PactException
                     (Vector (Maybe (T2 BlockHeight BlockHash)))))
lookupPactTxs restorePoint txs reqQ = do
    resultVar <- newEmptyMVar
    let !req = LookupPactTxsReq restorePoint txs resultVar
    let !msg = LookupPactTxsMsg req
    addRequest reqQ msg
    return resultVar
