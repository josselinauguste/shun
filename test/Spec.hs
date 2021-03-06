import           Data.ByteString                ( isPrefixOf )
import           Data.ByteString.Char8          ( pack )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( UTCTime(..)
                                                , secondsToDiffTime
                                                )
import           Test.Hspec

import           Ledger
import           Network
import           Proof
import           Transaction

time :: UTCTime
time =
  UTCTime {utctDay = fromGregorian 1984 4 28, utctDayTime = secondsToDiffTime 0}

oneBlockLedger :: HashConstraint -> BlockSize -> Ledger
oneBlockLedger hashConstraint blockSize = do
  let ledger            = makeLedger blockSize
  let (Just blockProof) = proofOfWork hashConstraint $ Just $ lastProof ledger
  let transaction       = Transaction
  validateTransactions time blockProof (appendTransaction transaction ledger)

main :: IO ()
main = hspec $ do
  let simpleHashConstraint = isPrefixOf (pack "0")
  let blockSize            = BlockSize 1
  let ledger               = makeLedger blockSize

  describe "Ledger" $ do
    describe "Make ledger"
      $          it "initializes ledger with genesis block"
      $          show (NonEmpty.head $ blocks $ makeLedger blockSize)
      `shouldBe` "Genesis"

    describe "Append transaction to the pool"
      $ it "returns the new ledger with the transaction"
      $ do
          let transaction   = Transaction
          let updatedLedger = appendTransaction transaction ledger
          head (pendingTransactions updatedLedger) `shouldBe` transaction

    describe "Create a block" $ do
      let (Just expectedProof) =
            proofOfWork simpleHashConstraint $ Just $ lastProof ledger
      it "returns the validated block and reinitializes transactions pool" $ do
        let expectedTransaction = Transaction
        let updatedLedger = validateTransactions
              time
              expectedProof
              (appendTransaction expectedTransaction ledger)
        length (blocks updatedLedger) `shouldBe` (2 :: Int)
        let (Block newBlockHeader newBlockTransactions) =
              NonEmpty.head $ blocks updatedLedger
        index newBlockHeader `shouldBe` (2 :: Int)
        timestamp newBlockHeader `shouldBe` time
        proof newBlockHeader `shouldBe` expectedProof
        newBlockTransactions `shouldBe` [expectedTransaction]
      it "only takes block size transactions from pending ones" $ do
        let (expectedTransaction1, expectedTransaction2) =
              (Transaction, Transaction)
        let ledgerWithTransactions =
              appendTransaction expectedTransaction2
                $ appendTransaction expectedTransaction1 ledger
        let updatedLedger =
              validateTransactions time expectedProof ledgerWithTransactions
        let (Block _newBlockHeader newBlockTransactions) =
              NonEmpty.head $ blocks updatedLedger
        newBlockTransactions `shouldBe` [expectedTransaction1]
        pendingTransactions updatedLedger `shouldBe` [expectedTransaction2]

    describe "Validate ledger" $ do
      it "returns true for an empty ledger"
        $          isValid simpleHashConstraint ledger
        `shouldBe` True
      it "returns true for a valid one"
        $          isValid simpleHashConstraint
                           (oneBlockLedger simpleHashConstraint blockSize)
        `shouldBe` True
      it "returns false for an invalid one"
        $          isValid (isPrefixOf (pack "Z"))
                           (oneBlockLedger simpleHashConstraint blockSize)
        `shouldBe` False

  describe "Proof of work" $ do
    it "computes proof for the first block with a very low hash constraint"
      $ show (proofOfWork simpleHashConstraint $ Just $ lastProof ledger)
      `shouldBe` "Just (Proof 172)"

    it "interspercing a block in valid ledger invalidates the ledger" $ pending

    it "modifying transactions in a validated block invalidates the ledger"
      $ pending

  describe "Network" $ do
    it "register a node to the network" $ do
      let newNode = Node ledger
      NonEmpty.head
          (nodes $ registerNodes (NonEmpty.fromList [newNode])
                                 (Network (NonEmpty.fromList []))
          )
        `shouldBe` newNode
    it "resolve conflict when all nodes have the same ledger" $ do
      let node1 = Node ledger
      let node2 = Node ledger
      resolveConflict simpleHashConstraint
                      (Network (NonEmpty.fromList [node1, node2]))
        `shouldBe` Just ledger
    it "resolve conflict when a node has a longer ledger" $ do
      let node1        = Node ledger
      let longerLedger = oneBlockLedger simpleHashConstraint blockSize
      let node2        = Node longerLedger
      resolveConflict simpleHashConstraint
                      (Network (NonEmpty.fromList [node1, node2]))
        `shouldBe` Just longerLedger
    it "only returns a valid ledger" $ do
      let node1 = Node ledger
      let (Just invalidProof) = proofOfWork
            simpleHashConstraint
            (proofOfWork simpleHashConstraint $ Just $ lastProof ledger)
      let invalidLedger = validateTransactions
            time
            invalidProof
            (appendTransaction Transaction ledger)
      let node2 = Node invalidLedger
      resolveConflict simpleHashConstraint
                      (Network (NonEmpty.fromList [node1, node2]))
        `shouldBe` Just ledger
    it "returns nothing when there is no valid ledger" $ do
      let (Just invalidProof) = proofOfWork
            simpleHashConstraint
            (proofOfWork simpleHashConstraint $ Just $ lastProof ledger)
      let invalidLedger = validateTransactions
            time
            invalidProof
            (appendTransaction Transaction ledger)
      let invalidNode = Node invalidLedger
      resolveConflict simpleHashConstraint
                      (Network (NonEmpty.fromList [invalidNode]))
        `shouldBe` Nothing
