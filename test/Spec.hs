import Control.Exception (evaluate)
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import Blockchain

main :: IO ()
main =
  hspec $ do
    describe "append transaction to the chain" $
      it "returns the new blockchain with the transaction" $ do
        let transaction = Transaction
        let blockchain = makeBlockchain
        let updatedBlockchain = appendTransaction transaction blockchain
        head (currentTransactions updatedBlockchain) `shouldBe` transaction
    describe "create a block" $
      it "returns the validated block and reinitialize transactions" $ do
        let blockchain = makeBlockchain
        let proof = _
        timestamp <- getCurrentTime
        let updatedBlockchain = createBlock timestamp proof blockchain
        length (blocks updatedBlockchain) `shouldBe` (1 :: Int)
        let block = head $ blocks $ updatedBlockchain
        index block `shouldBe` (1 :: Int)
        Blockchain.timestamp block `shouldBe` timestamp
        transactions block `shouldBe` []
        Blockchain.proof block `shouldBe` proof
