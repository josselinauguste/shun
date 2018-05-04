module Blockchain
  ( appendTransaction
  , makeBlockchain
  , createBlock
  , lastProof
  , Block(Block)
  , BlockHeader(index, timestamp, proof)
  , BlockSize(BlockSize)
  , Transaction(..)
  , Blockchain(blocks, pendingTransactions)
  )
where

import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Time.Clock                ( UTCTime )
import           Proof                          ( Hash(..)
                                                , Proof
                                                )

data Transaction =
  Transaction
  deriving (Eq, Show)

data BlockHeader = BlockHeader
  { index :: Int
  , timestamp :: UTCTime
  , proof :: Proof
  , previousHash :: Hash
  } deriving (Eq, Show)

data Block
  = Block BlockHeader
          [Transaction]
  | Genesis
  deriving (Eq, Show)

newtype BlockSize =
  BlockSize Int
  deriving (Eq, Show)

data Blockchain = Blockchain
  { blocks :: NonEmpty.NonEmpty Block
  , pendingTransactions :: [Transaction]
  , blockSize :: BlockSize
  } deriving (Eq, Show)

makeBlockchain :: BlockSize -> Blockchain
makeBlockchain chainBlockSize = Blockchain
  { blocks              = NonEmpty.fromList [Genesis]
  , pendingTransactions = []
  , blockSize           = chainBlockSize
  }

appendTransaction :: Transaction -> Blockchain -> Blockchain
appendTransaction transaction blockchain = blockchain
  { pendingTransactions = transaction : pendingTransactions blockchain
  }

createBlock :: UTCTime -> Proof -> Blockchain -> Blockchain
createBlock blockTimestamp blockProof blockchain = Blockchain
  { blocks              = NonEmpty.cons newBlock (blocks blockchain)
  , pendingTransactions = drop usedBlockSize (pendingTransactions blockchain)
  , blockSize           = blockSize blockchain
  }
 where
  newBlockIndex             = NonEmpty.length (blocks blockchain) + 1
  (BlockSize usedBlockSize) = blockSize blockchain
  newBlock                  = Block
    BlockHeader
      { index        = newBlockIndex
      , timestamp    = blockTimestamp
      , proof        = blockProof
      , previousHash = hash $ lastBlock blockchain
      }
    (take usedBlockSize (pendingTransactions blockchain))

hash :: Block -> Hash
hash _ = Hash

lastProof :: Blockchain -> Maybe Proof
lastProof blockchain = case block of
  Genesis               -> Nothing
  (Block blockHeader _) -> Just (proof blockHeader)
  where block = lastBlock blockchain

lastBlock :: Blockchain -> Block
lastBlock blockchain = NonEmpty.head $ blocks blockchain
