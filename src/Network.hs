module Network
  ( registerNodes
  , resolveConflict
  , Network(Network, nodes)
  , Node(Node)
  )
where

import           Data.Foldable      (maximumBy)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord           (comparing)
import           Ledger             (Ledger, blocks, isValid)
import           Proof              (HashConstraint)

newtype Node = Node
  { ledger :: Ledger
  } deriving (Eq, Show)

newtype Network = Network
  { nodes :: NonEmpty.NonEmpty Node
  } deriving (Eq, Show)

registerNodes :: NonEmpty.NonEmpty Node -> Network -> Network
registerNodes nodesToRegister network = network
  { nodes = NonEmpty.fromList
              (  (NonEmpty.toList nodesToRegister)
              ++ (NonEmpty.toList (nodes network))
              )
  }

resolveConflict :: HashConstraint -> Network -> Maybe Ledger
resolveConflict constraint network
  | null ledgers = Nothing
  | otherwise    = Just (maximumBy comparingLength ledgers)
 where
  comparingLength = comparing (NonEmpty.length . blocks)
  ledgers =
    filter (isValid constraint) (ledger <$> NonEmpty.toList (nodes network))
