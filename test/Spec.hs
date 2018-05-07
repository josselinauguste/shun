import           Data.ByteString                ( isPrefixOf )
import           Data.ByteString.Char8          ( pack )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Time.Clock                ( getCurrentTime )
import           Test.Hspec

import           Ledger
import           Proof
import           Network
import           Transaction

main :: IO ()
main = hspec $ do
  let simpleHashConstraint = isPrefixOf (pack "0")
  let blockSize            = BlockSize 1
  let ledger               = makeLedger blockSize

  describe "make ledger"
    $          it "initializes ledger with genesis block"
    $          show (NonEmpty.head $ blocks $ makeLedger blockSize)
    `shouldBe` "Genesis"

  describe "append transaction to the chain"
    $ it "returns the new ledger with the transaction"
    $ do
        let transaction   = Transaction
        let updatedLedger = appendTransaction transaction ledger
        head (pendingTransactions updatedLedger) `shouldBe` transaction

  describe "create a block" $ do
    let (Just expectedProof) =
          proofOfWork simpleHashConstraint $ lastProof ledger
    it "returns the validated block and reinitialize transactions" $ do
      expectedTimestamp <- getCurrentTime
      let expectedTransaction = Transaction
      let updatedLedger = validateTransactions
            expectedTimestamp
            expectedProof
            (appendTransaction expectedTransaction ledger)
      length (blocks updatedLedger) `shouldBe` (2 :: Int)
      let (Block newBlockHeader newBlockTransactions) =
            NonEmpty.head $ blocks updatedLedger
      index newBlockHeader `shouldBe` (2 :: Int)
      timestamp newBlockHeader `shouldBe` expectedTimestamp
      proof newBlockHeader `shouldBe` expectedProof
      newBlockTransactions `shouldBe` [expectedTransaction]
    it "only takes block size transactions from pending ones" $ do
      expectedTimestamp <- getCurrentTime
      let (expectedTransaction1, expectedTransaction2) =
            (Transaction, Transaction)
      let ledgerWithTransactions =
            appendTransaction expectedTransaction2
              $ appendTransaction expectedTransaction1 ledger
      let updatedLedger = validateTransactions expectedTimestamp
                                               expectedProof
                                               ledgerWithTransactions
      let (Block _newBlockHeader newBlockTransactions) =
            NonEmpty.head $ blocks updatedLedger
      newBlockTransactions `shouldBe` [expectedTransaction1]
      pendingTransactions updatedLedger `shouldBe` [expectedTransaction2]

  describe "proof of work"
    $ it "computes proof for the first block with a very low hash constraint"
    $ show (proofOfWork simpleHashConstraint $ lastProof ledger)
    `shouldBe` "Just (Proof 172)"

  describe "Network" $ do
    it "register a node to the network" $ do
      let newNode = Node ledger
      head (nodes $ registerNodes [newNode] (Network [])) `shouldBe` newNode
    it "resolve conflict when all nodes have the same ledger" $ do
      let node1 = Node ledger
      let node2 = Node ledger
      resolveConflict (Network [node1, node2]) `shouldBe` ledger
    it "resolve conflict when a node has a longer ledger" $ do
      let node1 = Node ledger
      expectedTimestamp <- getCurrentTime
      let (Just updatedLedgerProof) =
            proofOfWork simpleHashConstraint $ lastProof ledger
      let updatedLedger = validateTransactions
            expectedTimestamp
            updatedLedgerProof
            (appendTransaction Transaction ledger)
      let node2 = Node updatedLedger
      resolveConflict (Network [node1, node2]) `shouldBe` updatedLedger
