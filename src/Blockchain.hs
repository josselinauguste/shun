module Blockchain
  ( appendTransaction
  , makeBlockchain
  , Transaction(..)
  , Blockchain(blocks, currentTransactions)
  ) where

import Data.Time.Clock (UTCTime)

data Proof =
  Proof

data Hash =
  Hash

data Transaction =
  Transaction
  deriving (Eq, Show)

data Block = Block
  { index :: Int
  , timestamp :: UTCTime
  , transactions :: [Transaction]
  , proof :: Proof
  , previousHash :: Hash
  }

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
