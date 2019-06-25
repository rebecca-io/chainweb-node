{-# OPTIONS_GHC -Wwarn #-}   -- FIXME

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

module Main (main) where

import Configuration.Utils hiding (Lens', (<.>))
import Control.Lens hiding ((.=), (<.>))
import Data.Foldable
import GHC.Generics
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS as WARP (runTLSSocket)
import PkgInfo
import qualified System.Logger as L
import Utils.Logging.Config

import Chainweb.ChainId
import Chainweb.Chainweb
import Chainweb.Logger
import qualified Chainweb.Mempool.InMem as Mempool
import Chainweb.Mempool.Mempool (Mempool)
import Chainweb.Mempool.P2pConfig
import Chainweb.Mempool.RestAPI
import Chainweb.Mempool.RestAPI.Server
import Chainweb.Node (runRtsMonitor, withNodeLogger)
import Chainweb.RestAPI
import Chainweb.RestAPI.Utils
import Chainweb.Version
import Network.X509.SelfSigned

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

defaultMempoolNodeConfiguration :: ChainwebVersion -> MempoolNodeConfiguration
defaultMempoolNodeConfiguration v = MempoolNodeConfiguration
    { _mpConfigLog = defaultLogConfig & logConfigLogger . L.loggerConfigThreshold .~ L.Info
    , _mpChainwebConfig = defaultChainwebConfiguration v
    }

instance ToJSON MempoolNodeConfiguration where
    toJSON o = object
        [ "chainweb" .= _mpChainwebConfig o
        , "logging" .= _mpConfigLog o
        ]

instance FromJSON (MempoolNodeConfiguration -> MempoolNodeConfiguration) where
    parseJSON = withObject "MempoolNodeConfiguration" $ \o -> id
        <$< mpChainwebConfig %.: "chainweb" % o
        <*< mpConfigLog %.: "logging" % o

pMempoolNodeConfiguration :: MParser MempoolNodeConfiguration
pMempoolNodeConfiguration = id
    <$< mpChainwebConfig %:: pChainwebConfiguration
    <*< mpConfigLog %:: pLogConfig

serveMempoolSocketTls
    :: Settings
    -> X509CertChainPem
    -> X509KeyPem
    -> Socket
    -> (t -> Application)
    -> t
    -> IO ()
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
    undefined

  where
    conf = _mpChainwebConfig mpConf
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

-- -------------------------------------------------------------------------- --
-- main

mainInfo :: ProgramInfo MempoolNodeConfiguration
mainInfo = programInfo
    "Chainweb Mempool Node"
    pMempoolNodeConfiguration
    (defaultMempoolNodeConfiguration Testnet01)

main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \conf -> do
    let v = _configChainwebVersion $ _mpChainwebConfig conf
    withNodeLogger (_mpConfigLog conf) v $ node conf
