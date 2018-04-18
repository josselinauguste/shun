import Control.Exception (evaluate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import Blockchain
import Proof

main :: IO ()
main =
  hspec $ do
    describe "append transaction to the chain" $
      it "returns the new blockchain with the transaction" $ do
        let transaction = Transaction
        let blockchain = makeBlockchain
        let updatedBlockchain = appendTransaction transaction blockchain
        head (currentTransactions updatedBlockchain) `shouldBe` transaction
    describe "proof of work" $
      it "computes proof for the first block" $ do
        let blockchain = makeBlockchain
        show (proofOfWork $ lastProof blockchain) `shouldBe` ""
    describe "create a block" $
      it "returns the validated block and reinitialize transactions" $ do
        let blockchain = makeBlockchain
        let (Just proof) = proofOfWork $ lastProof blockchain
        timestamp <- getCurrentTime
        let updatedBlockchain = createBlock timestamp proof blockchain
        length (blocks updatedBlockchain) `shouldBe` (2 :: Int)
        let block = NonEmpty.head $ blocks updatedBlockchain
        index block `shouldBe` (2 :: Int)
        Blockchain.timestamp block `shouldBe` timestamp
        transactions block `shouldBe` []
        Blockchain.proof block `shouldBe` proof
