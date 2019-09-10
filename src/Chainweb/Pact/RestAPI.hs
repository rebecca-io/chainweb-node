{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Chainweb.Pact.RestAPI where


import Data.Singletons
import Data.Text (Text)

import Servant

-- internal chainweb modules

import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.Version

-- internal pact modules

import Pact.Types.Term (PactId(..))
import Pact.Server.API as API


-- -------------------------------------------------------------------------- --
-- Pact API -- @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/@

type PactApi_ = "pact" :> API.ApiV1API -- TODO unify with Pact versioning

type PactApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v
    :> ChainEndpoint c
    :> Reassoc PactApi_

pactApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactApi v c)
pactApi = Proxy

-- -------------------------------------------------------------------------- --
-- SPV API - @GET /chainweb/<ApiVersion>/<ChainwebVersion>/chain/<ChainId>/pact/cont

type PactSpvApi_
    = "pact"
    :> "cont"
    :> Capture "pactId" PactId
    :> Get '[PlainText] Text

type PactSpvApi (v :: ChainwebVersionT) (c :: ChainIdT)
    = 'ChainwebEndpoint v
    :> ChainEndpoint c
    :> Reassoc PactSpvApi_

pactSpvApi
    :: forall (v :: ChainwebVersionT) (c :: ChainIdT)
    . Proxy (PactSpvApi v c)
pactSpvApi = Proxy

-- -------------------------------------------------------------------------- --
-- Some Cut Api

type PactServiceApi v c
    = PactApi v c :<|> PactSpvApi v c

pactServiceApi :: forall (v :: ChainwebVersionT) (c :: ChainIdT) . Proxy (PactServiceApi v c)
pactServiceApi = Proxy


somePactApi :: ChainwebVersion -> ChainId -> SomeApi
somePactApi
    (FromSing (SChainwebVersion :: Sing v))
    (FromSing (SChainId :: Sing c))
    = SomeApi $ pactServiceApi @v @c

somePactApis :: ChainwebVersion -> [ChainId] -> SomeApi
somePactApis v = mconcat . fmap (somePactApi v)
