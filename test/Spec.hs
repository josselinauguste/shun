import Control.Exception (evaluate)
import Test.Hspec

import Blockchain

main :: IO ()
main =
  hspec $
  describe "append transaction to the chain" $
  it "returns the new blockchain with the transaction" $ do
    let updatedBlockchain = appendTransaction transaction blockchain
    head (currentTransactions updatedBlockchain) `shouldBe` transaction
  where
    transaction = Transaction
    blockchain = makeBlockchain
