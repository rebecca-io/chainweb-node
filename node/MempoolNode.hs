{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MempoolNode (main) where

import Configuration.Utils hiding (Lens', (<.>))
import Control.Lens hiding ((.=), (<.>))
import Network.Socket
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai.Handler.WarpTLS as WARP (runTLSSocket)
import GHC.Generics
import Utils.Logging
import Utils.Logging.Config

import Chainweb.Chainweb
import Chainweb.RestAPI
import Chainweb.RestAPI.Utils
import qualified Chainweb.Mempool.InMemTypes as Mempool
import qualified Chainweb.Mempool.Mempool as Mempool
import Chainweb.Mempool.P2pConfig
import ChainwebNode (runRtsMonitor)

data MempoolNodeConfiguration = MempoolNodeConfiguration
    { _mpConfigLog :: !LogConfig
    , _mpChainwebConfig :: !ChainwebConfiguration
       -- going to piggy-back this for now. TODO: include fields we actually
       -- use and port over the parsers
       --
       -- we want:
       --   - _configMempoolP2p
       --   - _configNodeId
       --   - _configP2p (maybe?)
    }
    deriving (Show, Eq, Generic)
makeLenses ''MempoolNodeConfiguration

instance ToJSON MempoolNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _mpConfigChainweb o
        , "logging" .= _mpConfigLog o
        ]

instance FromJSON (MempoolNodeConfiguration -> MempoolNodeConfiguration) where
    parseJSON = withObject "MempoolNodeConfiguration" $ \o -> id
        <$< mpConfigChainweb %.: "chainweb" % o
        <*< mpConfigLog %.: "logging" % o

pMempoolNodeConfiguration :: MParser MempoolNodeConfiguration
pMempoolNodeConfiguration = id
    <$< mpConfig %:: pChainwebConfiguration
    <$< mpConfigLog %:: pLogConfig

serveMempoolSocketTls settings certChain key sock m app
    = runTLSSocket tlsSettings settings sock $ m app
  where
    tlsSettings = tlsServerChainSettings certChain key

runMempoolNode
    :: Logger logger
    => MempoolNodeConfiguration
    -> logger
    -> IO ()
runMempoolNode mpConf logger = withMempools $ \mempools -> do
    let app = chainwebCors $ someServerApplication
                           $ someMempoolServers v mempools

  where
    conf = _mpConfigChainweb mpConf
    v = _configChainwebVersion conf
    cids = chainIds v
    cwnid = _configNodeId conf
    mempoolConf = mempoolConfig False -- reintro run by consensus node

    withMP = Mempool.withInMemoryMempool mempoolConf
    withFoldFunc f cid = \mps -> withMP (\mp -> f ((cid, mp):mps))
    with userFunc = foldl' withFoldFunc userFunc cids

    withMempools :: ([(ChainId, Mempool ChainwebTransaction)] -> IO a)
                 -> IO a
    withMempools userFunc = with userFunc []

node :: Logger logger => MempoolNodeConfiguration -> logger -> IO ()
node conf logger = undefined
