module Blockchain
  ( appendTransaction
  , makeBlockchain
  , createBlock
  , Block(index, timestamp, transactions, proof)
  , Transaction(..)
  , Blockchain(blocks, currentTransactions)
  ) where

import Data.Time.Clock (UTCTime)

data Proof =
  Proof
  deriving (Eq, Show)

data Hash =
  Hash
  deriving (Eq, Show)

data Transaction =
  Transaction
  deriving (Eq, Show)

data Block = Block
  { index :: Int
  , timestamp :: UTCTime
  , transactions :: [Transaction]
  , proof :: Proof
  , previousHash :: Hash
  } deriving (Eq, Show)

data Blockchain = Blockchain
  { blocks :: [Block]
  , currentTransactions :: [Transaction]
  }

makeBlockchain :: Blockchain
makeBlockchain = Blockchain {blocks = [], currentTransactions = []}

appendTransaction :: Transaction -> Blockchain -> Blockchain
appendTransaction transaction blockchain =
  blockchain
  {currentTransactions = transaction : currentTransactions blockchain}

createBlock :: UTCTime -> Proof -> Blockchain -> Blockchain
createBlock timestamp proof blockchain =
  Blockchain {blocks = newBlock : blocks blockchain, currentTransactions = []}
  where
    newBlockIndex = length (blocks blockchain) + 1
    newBlock =
      Block
      { index = newBlockIndex
      , timestamp = timestamp
      , transactions = currentTransactions blockchain
      , proof = proof
      , previousHash = hash $ lastBlock blockchain
      }

hash :: Block -> Hash
hash _ = Hash

lastBlock :: Blockchain -> Block
lastBlock blockchain = head $ blocks blockchain
