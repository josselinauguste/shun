module Ledger
  ( appendTransaction
  , makeLedger
  , validateTransactions
  , lastProof
  , Block(Block)
  , BlockHeader(index, timestamp, proof)
  , BlockSize(BlockSize)
  , Ledger(blocks, pendingTransactions)
  )
where

import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Time.Clock                ( UTCTime )
import           Proof                          ( Hash(..)
                                                , Proof
                                                )
import           Transaction                    ( Transaction )

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

data Ledger = Ledger
  { blocks :: NonEmpty.NonEmpty Block
  , pendingTransactions :: [Transaction]
  , blockSize :: BlockSize
  } deriving (Eq, Show)

makeLedger :: BlockSize -> Ledger
makeLedger chainBlockSize = Ledger
  { blocks              = NonEmpty.fromList [Genesis]
  , pendingTransactions = []
  , blockSize           = chainBlockSize
  }

appendTransaction :: Transaction -> Ledger -> Ledger
appendTransaction transaction ledger = ledger
  { pendingTransactions = transaction : pendingTransactions ledger
  }

validateTransactions :: UTCTime -> Proof -> Ledger -> Ledger
validateTransactions blockTimestamp blockProof ledger = Ledger
  { blocks              = NonEmpty.cons newBlock (blocks ledger)
  , pendingTransactions = drop usedBlockSize (pendingTransactions ledger)
  , blockSize           = blockSize ledger
  }
 where
  newBlockIndex             = NonEmpty.length (blocks ledger) + 1
  (BlockSize usedBlockSize) = blockSize ledger
  newBlock                  = Block
    BlockHeader
      { index        = newBlockIndex
      , timestamp    = blockTimestamp
      , proof        = blockProof
      , previousHash = hash $ lastBlock ledger
      }
    (take usedBlockSize (pendingTransactions ledger))

hash :: Block -> Hash
hash _ = Hash

lastProof :: Ledger -> Maybe Proof
lastProof ledger = case block of
  Genesis               -> Nothing
  (Block blockHeader _) -> Just (proof blockHeader)
  where block = lastBlock ledger

lastBlock :: Ledger -> Block
lastBlock ledger = NonEmpty.head $ blocks ledger
