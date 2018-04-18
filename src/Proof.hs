module Proof
  ( proofOfWork
  , Proof
  , Hash(Hash)
  ) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString (isPrefixOf)
import Data.ByteString.Char8 (pack)
import Data.List (find)

newtype Proof =
  Proof Int
  deriving (Eq, Show)

data Hash =
  Hash
  deriving (Eq, Show)

proofOfWork :: Proof -> Maybe Proof
proofOfWork previousProof = find (validProof previousProof) potentialProofs
  where
    potentialProofs = [Proof i | i <- [1 ..]]

validProof :: Proof -> Proof -> Bool
validProof (Proof previousProof) (Proof proofToValidate) =
  pack "0000" `isPrefixOf`
  hash (pack $ show previousProof ++ show proofToValidate)
