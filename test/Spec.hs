import Data.ByteString (isPrefixOf)
import Data.ByteString.Char8 (pack)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import Blockchain
import Proof

main :: IO ()
main =
  hspec $ do
    let simpleHashConstraint = isPrefixOf (pack "0")
    describe "append transaction to the chain" $
      it "returns the new blockchain with the transaction" $ do
        let transaction = Transaction
        let blockchain = makeBlockchain
        let updatedBlockchain = appendTransaction transaction blockchain
        head (currentTransactions updatedBlockchain) `shouldBe` transaction
    describe "proof of work" $
      it "computes proof for the first block with a very low hash constraint" $ do
        let blockchain = makeBlockchain
        show (proofOfWork simpleHashConstraint $ lastProof blockchain) `shouldBe`
          "Just (Proof 172)"
    describe "create a block" $
      it "returns the validated block and reinitialize transactions" $ do
        let blockchain = makeBlockchain
        let (Just expectedProof) =
              proofOfWork simpleHashConstraint $ lastProof blockchain
        expectedTimestamp <- getCurrentTime
        let updatedBlockchain =
              createBlock expectedTimestamp expectedProof blockchain
        length (blocks updatedBlockchain) `shouldBe` (2 :: Int)
        let block = NonEmpty.head $ blocks updatedBlockchain
        index block `shouldBe` (2 :: Int)
        timestamp block `shouldBe` expectedTimestamp
        transactions block `shouldBe` []
        proof block `shouldBe` expectedProof
