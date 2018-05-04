import           Data.ByteString                ( isPrefixOf )
import           Data.ByteString.Char8          ( pack )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Time.Clock                ( getCurrentTime )
import           Test.Hspec

import           Blockchain
import           Proof

main :: IO ()
main = hspec $ do
  let simpleHashConstraint = isPrefixOf (pack "0")
  let blockSize            = BlockSize 1
  let blockchain           = makeBlockchain blockSize
  describe "make blockchain"
    $          it "initializes blockchain with genesis block"
    $          show (NonEmpty.head $ blocks $ makeBlockchain blockSize)
    `shouldBe` "Genesis"
  describe "append transaction to the chain"
    $ it "returns the new blockchain with the transaction"
    $ do
        let transaction       = Transaction
        let updatedBlockchain = appendTransaction transaction blockchain
        head (pendingTransactions updatedBlockchain) `shouldBe` transaction
  describe "create a block" $ do
    let (Just expectedProof) =
          proofOfWork simpleHashConstraint $ lastProof blockchain
    it "returns the validated block and reinitialize transactions" $ do
      expectedTimestamp <- getCurrentTime
      let expectedTransaction = Transaction
      let updatedBlockchain = createBlock
            expectedTimestamp
            expectedProof
            (appendTransaction expectedTransaction blockchain)
      length (blocks updatedBlockchain) `shouldBe` (2 :: Int)
      let (Block newBlockHeader newBlockTransactions) =
            NonEmpty.head $ blocks updatedBlockchain
      index newBlockHeader `shouldBe` (2 :: Int)
      timestamp newBlockHeader `shouldBe` expectedTimestamp
      proof newBlockHeader `shouldBe` expectedProof
      newBlockTransactions `shouldBe` [expectedTransaction]
    it "only takes block size transactions from pending ones" $ do
      expectedTimestamp <- getCurrentTime
      let (expectedTransaction1, expectedTransaction2) =
            (Transaction, Transaction)
      let blockchainWithTransactions =
            appendTransaction expectedTransaction2
              $ appendTransaction expectedTransaction1 blockchain
      let updatedBlockchain = createBlock expectedTimestamp
                                          expectedProof
                                          blockchainWithTransactions
      let (Block _newBlockHeader newBlockTransactions) =
            NonEmpty.head $ blocks updatedBlockchain
      newBlockTransactions `shouldBe` [expectedTransaction1]
      pendingTransactions updatedBlockchain `shouldBe` [expectedTransaction2]
  describe "proof of work"
    $ it "computes proof for the first block with a very low hash constraint"
    $ show (proofOfWork simpleHashConstraint $ lastProof blockchain)
    `shouldBe` "Just (Proof 172)"
