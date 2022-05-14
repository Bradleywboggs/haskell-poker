
import           Test.QuickCheck (property, choose)
import           Test.Hspec 
import           Data.PokerHand
import           PokerHandBuilderTest (testBuilder)
import           PokerHandTest        (testPokerHand) 
import           Data.List            (nub)

main :: IO ()
main = hspec $ do
    testBuilder
    testPokerHand
    describe "Deck" $ do
      it "does not contain duplicate cards" $ property $ do
        d <- shuffleDeck deck
        (length . unwrapDeck) d `shouldBe` (length . nub . unwrapDeck) d
      
                

