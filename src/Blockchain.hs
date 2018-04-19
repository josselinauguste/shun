module Blockchain
  ( appendTransaction
  , makeBlockchain
  , createBlock
  , lastProof
  , Block(index, timestamp, transactions, proof)
  , Transaction(..)
  , Blockchain(blocks, currentTransactions)
  ) where

import Data.List.NonEmpty as NonEmpty
import Data.Time.Clock (UTCTime)
import Proof (Hash(..), Proof)

data Transaction =
  Transaction
  deriving (Eq, Show)

data Block
  = Block { index :: Int
          , timestamp :: UTCTime
          , transactions :: [Transaction]
          , proof :: Proof
          , previousHash :: Hash }
  | Genesis
  deriving (Eq, Show)

data Blockchain = Blockchain
  { blocks :: NonEmpty Block
  , currentTransactions :: [Transaction]
  }

makeBlockchain :: Blockchain
makeBlockchain =
  Blockchain {blocks = NonEmpty.fromList [Genesis], currentTransactions = []}

appendTransaction :: Transaction -> Blockchain -> Blockchain
appendTransaction transaction blockchain =
  blockchain
  {currentTransactions = transaction : currentTransactions blockchain}

createBlock :: UTCTime -> Proof -> Blockchain -> Blockchain
createBlock blockTimestamp blockProof blockchain =
  Blockchain
  { blocks = NonEmpty.cons newBlock (blocks blockchain)
  , currentTransactions = []
  }
  where
    newBlockIndex = NonEmpty.length (blocks blockchain) + 1
    newBlock =
      Block
      { index = newBlockIndex
      , timestamp = blockTimestamp
      , transactions = currentTransactions blockchain
      , proof = blockProof
      , previousHash = hash $ lastBlock blockchain
      }

hash :: Block -> Hash
hash _ = Hash

lastProof :: Blockchain -> Maybe Proof
lastProof blockchain =
  case block of
    Genesis -> Nothing
    _ -> Just (proof block)
  where
    block = lastBlock blockchain

lastBlock :: Blockchain -> Block
lastBlock blockchain = NonEmpty.head $ blocks blockchain
