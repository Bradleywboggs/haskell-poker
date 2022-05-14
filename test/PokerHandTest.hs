module PokerHandTest where

import Data.Cards
import Data.PokerHand
import Data.PokerHand.Builder
import TestData
import Test.Hspec

testPokerHand :: SpecWith ()
testPokerHand = do
    describe "toHand (creating Poker Hand)" $ do
       it "returns RoyalFlush with royal flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards royalFlushCards)) in
          toHand cards `shouldBe` RoyalFlush (PokerCards sortedRoyalFlushCards)
       it "returns StraightFlush with straight flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards straightFlushCards)) in
          toHand cards `shouldBe` StraightFlush (HighCardRank Six) cards
       it "returns FourOfAKind with four of a kind cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards fourofAKindCards)) in
          toHand cards `shouldBe` FourOfAKind (FourOfAKindRank Ten) (HighCardRank Seven) cards
       it "returns Flush with flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards flushCards)) in
          toHand cards `shouldBe` Flush (HighCardRank Nine) cards
       it "returns Straight with straight cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards straightCards)) in
          toHand cards `shouldBe` Straight (HighCardRank King) cards
       it "returns FullHouse with fullhouse cards ( lows of high ) " $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards fullHouseCardsLowsOfHighs)) in
          toHand cards `shouldBe` FullHouse (ThreeOfAKindRank Ten) (PairRank King) cards
       it "returns FullHouse with fullhouse cards ( highs  of low ) " $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards fullHouseCardsHighsOfLows)) in
          toHand cards `shouldBe` FullHouse (ThreeOfAKindRank King) (PairRank Ten) cards
       it "returns ThreeOfAKind with three of a kind cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards threeOfAKindCards)) in
          toHand cards `shouldBe` ThreeOfAKind (ThreeOfAKindRank Ten) (HighCardRank King) (Kicker Jack) cards
       it "returns Pair with pair cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards pairCards)) in
          toHand cards `shouldBe` Pair (PairRank Ten) (HighCardRank King) (Kicker Jack) cards
       it "returns HighCard with crap cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards crapCards)) in
          toHand cards `shouldBe` HighCard (HighCardRank Eight) (Kicker Six) cards
       it "returns TwoPair with three of a two pair cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards twoPairCards)) in
          toHand cards `shouldBe` TwoPair (PairRank King) (PairRank Ten) (HighCardRank Jack) cards
    describe "simple hand comparisons (>, <)" $ do
      describe "RoyalFlush" $ do
        it "beats StraightFlush" $
          let
            rfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards royalFlushCards))
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards straightFlushCards))
          in
          RoyalFlush rfcards > StraightFlush (HighCardRank Six) sfcards `shouldBe` True
      describe "StraightFlush" $ do
        it "loses to RoyalFlush" $
          let
            rfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards royalFlushCards))
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards straightFlushCards))
          in
          StraightFlush (HighCardRank Six) sfcards < RoyalFlush rfcards `shouldBe` True
        it "beats FourOfAKind" $
          let
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards straightFlushCards))
            foak = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards fourofAKindCards))
          in
          StraightFlush (HighCardRank Six) sfcards > FourOfAKind (FourOfAKindRank Ten) (HighCardRank Seven) foak `shouldBe` True    
    describe "show Hand" $ do   
      describe "show FullHouse" $ do
        it "displays FullHouse, low full of high" $ 
          let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards (PossiblePokerCards fullHouseCardsLowsOfHighs)) in
          show (toHand cards) `shouldBe` " [ 10\9827\65039 ] [ 10\9829\65039 ] [ 10\9830\65039 ] [ K\9829\65039 ] [ K\9827\65039 ]\nFull House, 10's full of K's"
        -- it "displays FullHouse, low full of high" $ 
        --     let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fullHouseCardsLowsOfHighs) in
        --     show (hand cards) `shouldBe` ""
