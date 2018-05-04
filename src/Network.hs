module Network
  ( registerNodes
  , resolveConflict
  , Network(Network, nodes)
  , Node(Node)
  )
where

import           Blockchain
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Foldable                  ( maximumBy )
import           Data.Ord                       ( comparing )

newtype Node = Node
  { blockchain :: Blockchain
  } deriving (Eq, Show)

newtype Network = Network
  { nodes :: [Node]
  } deriving (Eq, Show)

registerNodes :: [Node] -> Network -> Network
registerNodes nodesToRegister network =
  network { nodes = nodesToRegister ++ nodes network }

resolveConflict :: Network -> Blockchain
resolveConflict network = maximumBy comparingBlockchainLength blockchains
 where
  comparingBlockchainLength = comparing (NonEmpty.length . blocks)
  blockchains = blockchain <$> nodes network
