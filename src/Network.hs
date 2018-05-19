module Network
  ( registerNodes
  , resolveConflict
  , Network(Network, nodes)
  , Node(Node)
  )
where

import           Ledger                         ( Ledger
                                                , blocks
                                                , isValid
                                                )
import           Proof                          ( HashConstraint )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Foldable                  ( maximumBy )
import           Data.Ord                       ( comparing )

newtype Node = Node
  { ledger :: Ledger
  } deriving (Eq, Show)

newtype Network = Network
  { nodes :: [Node]
  } deriving (Eq, Show)

registerNodes :: [Node] -> Network -> Network
registerNodes nodesToRegister network =
  network { nodes = nodesToRegister ++ nodes network }

resolveConflict :: HashConstraint -> Network -> Ledger
resolveConflict constraint network = maximumBy comparingLength ledgers
 where
  comparingLength = comparing (NonEmpty.length . blocks)
  ledgers         = filter (isValid constraint) (ledger <$> nodes network)
