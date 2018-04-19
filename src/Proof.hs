module Proof
  ( proofOfWork
  , defaultHashConstraint
  , Proof
  , Hash(Hash)
  ) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Char8 (pack)
import Data.List (find)

newtype Proof =
  Proof Int
  deriving (Eq, Show)

data Hash =
  Hash
  deriving (Eq, Show)

type HashConstraint = ByteString -> Bool

proofOfWork :: HashConstraint -> Maybe Proof -> Maybe Proof
proofOfWork constraint Nothing = computeProofOfWork constraint genesisProof
proofOfWork constraint (Just previousProof) =
  computeProofOfWork constraint previousProof

genesisProof :: Proof
genesisProof = Proof 100

computeProofOfWork :: HashConstraint -> Proof -> Maybe Proof
computeProofOfWork constraint previousProof =
  find (validProof constraint previousProof) potentialProofs
  where
    potentialProofs = [Proof i | i <- [1 ..]]

validProof :: HashConstraint -> Proof -> Proof -> Bool
validProof constraint (Proof previousProof) (Proof proofToValidate) =
  constraint (hash $ pack $ show previousProof ++ show proofToValidate)

defaultHashConstraint :: HashConstraint
defaultHashConstraint = isPrefixOf (pack "0000")
