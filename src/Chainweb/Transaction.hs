{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Transaction
  ( ChainwebTransaction
  , HashableTrans(..)
  , PayloadWithText(..)
  , chainwebPayloadCodec
  , gasLimitOf
  , gasPriceOf
  ) where

import Control.DeepSeq

import qualified Data.ByteString.Char8 as B
import Data.Hashable

import qualified Data.Aeson as Aeson
import Data.Bytes.Get
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Short as SB
import Data.Text (Text)

import GHC.Generics (Generic)

import Pact.Parse (parseExprs)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas (GasLimit(..), GasPrice(..))
import Pact.Types.Hash

import Chainweb.Utils

-- | A product type representing a `Payload PublicMeta ParsedCode` coupled with
-- the Text that generated it, to make gossiping easier.
--
data PayloadWithText = PayloadWithText
    { payloadBytes :: !SB.ShortByteString
    , payloadObj :: !(Payload PublicMeta ParsedCode)
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

{-
instance ToJSON PayloadWithText where
    toJSON (PayloadWithText bs _) = toJSON (T.decodeUtf8 $ SB.fromShort bs)
    toEncoding (PayloadWithText bs _) = toEncoding (T.decodeUtf8 $ SB.fromShort bs)

instance FromJSON PayloadWithText where
    parseJSON = Aeson.withText "PayloadWithText" $ \text -> let bs = T.encodeUtf8 text in
        case traverse parsePact =<< Aeson.eitherDecodeStrict' bs of
          Left err -> fail err
          (Right !payload) -> pure $! PayloadWithText (SB.toShort bs) (force payload)
      where
        parsePact :: Text -> Either String ParsedCode
        parsePact code = ParsedCode code <$> parseExprs code
-}

type ChainwebTransaction = Command PayloadWithText

-- | Hashable newtype of ChainwebTransaction
newtype HashableTrans a = HashableTrans { unHashable :: Command a }
    deriving (Eq, Functor, Ord)

instance Hashable (HashableTrans PayloadWithText) where
    hashWithSalt s (HashableTrans t) = hashWithSalt s hashCode
      where
        (TypedHash hc) = _cmdHash t
        decHC = runGetS getWord64host
        !hashCode = either error id $ decHC (B.take 8 hc)
    {-# INLINE hashWithSalt #-}

-- | A codec for (Command PayloadWithText) transactions.
chainwebPayloadCodec :: Codec (Command PayloadWithText)
chainwebPayloadCodec = Codec enc dec
  where
    enc c = encodeToByteString $ fmap (SB.fromShort . payloadBytes) c
    dec bs = case Aeson.decodeStrict' bs of
               Just cmd -> traverse decodePayload cmd
               Nothing -> Left "decode PayloadWithText failed"

decodePayload :: ByteString -> Either String PayloadWithText
decodePayload bs = case Aeson.decodeStrict' bs of
    Just payload -> do
        p <- traverse parsePact payload
        return $! PayloadWithText (SB.toShort bs) p
    Nothing -> Left "decoding Payload failed"

parsePact :: Text -> Either String ParsedCode
parsePact code = ParsedCode code <$> parseExprs code

-- | Get the gas limit/supply of a public chain command payload
gasLimitOf :: forall c. Command (Payload PublicMeta c) -> GasLimit
gasLimitOf = _pmGasLimit . _pMeta . _cmdPayload
{-# INLINE gasLimitOf #-}

-- | Get the gas price of a public chain command payload
gasPriceOf :: forall c. Command (Payload PublicMeta c) -> GasPrice
gasPriceOf = _pmGasPrice . _pMeta . _cmdPayload
{-# INLINE gasPriceOf #-}
