module PokerHandBuilderTest (testBuilder)

where

import Data.PokerHand
import Data.PokerHand.Builder
import Data.Cards
import Test.Hspec
import TestData

testBuilder :: SpecWith ()
testBuilder = do
  describe "Data.PokerHand.Builder" $ do
    describe "toPokerCards" $ do
      it "converts list of cards to Just sorted 5 player cards when cards are list of 5 unique card values" $
        toPokerCards (PossiblePokerCards straightFlushCards) `shouldBe` Just (PokerCards straightFlushCards)
      it "converts list of cards to Nothing  when cards are not of length 5 card values" $
        toPokerCards (PossiblePokerCards (Card (Hearts, King): straightFlushCards)) `shouldBe` Nothing
      it "converts list of cards to Nothing  when cards are length 5, but not unique card values" $
        toPokerCards (PossiblePokerCards [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, Ace), Card (Clubs, Ace)])  `shouldBe` Nothing
    describe "sortPokerCards" $ do
      it "sorts toPokerCards in ascending order" $
        sortPokerCards <$> toPokerCards (PossiblePokerCards royalFlushCards) `shouldBe` Just (PokerCards sortedRoyalFlushCards)
    describe "(.:.)(appending new card to existing matching card state)" $ do
      -- NoMatches Nothing
      it "creates a new PendingMatches NoMatches Just PendingMatch from the input card if Maybe PendingMatch is Nothing"  $
        PendingMatches NoMatches Nothing .:.  Three `shouldBe` PendingMatches NoMatches  (Just (PendingMatch Three))
      -- NoMatches Just, input rank doesn't match
      it "creates a new PendingMatches NoMatches Just PendingMatch from the input card if the card rank doesn't match existing PendingMatches NoMatches Just _"  $
        PendingMatches NoMatches (Just (PendingMatch Two)) .:.  Three `shouldBe` PendingMatches NoMatches  (Just (PendingMatch Three))
      -- NoMatches Just, input rank matches
      it "creates a PairMatch when there's an existing PendingMatches NoMatches Just and the new card rank is equal to the PendingMatch" $
        PendingMatches NoMatches (Just (PendingMatch Two)) .:.  Two `shouldBe` PendingMatches (PairMatch (PairRank Two)) Nothing

      -- PairMatch Nothing, input rank doesn't match
      it "adds a PendingMatches NoMatches Just PendingMatch with the existing PairMatch if the input card rank doesn't match the existing pair" $
        PendingMatches (PairMatch (PairRank Two)) Nothing .:.  Three `shouldBe` PendingMatches (PairMatch (PairRank Two)) (Just (PendingMatch Three))
      -- PairMatch Nothing, input rank matches
      it "creates a PendingMatches ThreeCardMatch Nothing when the rank of the existing PairMatch is equal to the input card rank" $
        PendingMatches (PairMatch (PairRank Two)) Nothing .:.  Two `shouldBe` PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) Nothing
      -- PairMatch Just, input rank doesn't match
      it "replaces PendingMatch rank with input card rank when PendingMatches TwoPairMatch Just PendingMatch if input card rank doesn't match" $
        PendingMatches (PairMatch (PairRank Two)) (Just (PendingMatch Three)) .:.  Four `shouldBe` PendingMatches (PairMatch (PairRank Two)) (Just (PendingMatch Four))
      -- PairMatch Just, input rank matches
      it "creates a TwoPairMatch if existing matches consist of a pair and pending single and the input card rank is equal to the pending single match" $
        PendingMatches (PairMatch (PairRank Two)) (Just (PendingMatch Three)) .:.  Three `shouldBe` PendingMatches (TwoPairMatch (PairRank Three) (PairRank Two)) Nothing

      -- TwoPairMatch Nothing, input rank doesn't match
      it "ignores input card when TwoPairMatch second PairRank doesn't match the input card" $
        PendingMatches (TwoPairMatch (PairRank Two) (PairRank Three)) Nothing .:.  Four `shouldBe` PendingMatches (TwoPairMatch (PairRank Two) (PairRank Three)) Nothing
      -- TwoPairMatch Nothing, input rank matches
      it "creates a PendingMatches FullHouseMatch Nothing if the latter existing TwoPair rank matches the input card rank" $
        PendingMatches (TwoPairMatch (PairRank Two) (PairRank Three)) Nothing .:.  Three `shouldBe` PendingMatches (FullHouseMatch (ThreeOfAKindRank Three) (PairRank Two)) Nothing
      -- TwoPairMatch Just, any input rank
      it "ignores input card when existing is Pending Matches TwoPairMatch Just PendingMatch" $
        PendingMatches (TwoPairMatch (PairRank Eight) (PairRank Nine)) (Just (PendingMatch Ten)) .:.  Ace `shouldBe` PendingMatches (TwoPairMatch (PairRank Eight) (PairRank Nine)) Nothing

      -- ThreeCardMatch Nothing, input rank doesn't match
      it "adds a  Just PendingMatch to the existing ThreeCardMatch if input card rank doesn't match" $
        PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) Nothing .:.  Three `shouldBe` PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) ( Just (PendingMatch Three))
      -- ThreeCardMatch Nothing, input rank doesn't match
      it "creates a PendingMatches FourOfAKind Nothing if the existing ThreeCardMatch rank is equal to the input card rank" $
        PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) Nothing .:.  Two `shouldBe` PendingMatches (FourCardMatch (FourOfAKindRank Two)) Nothing
      -- ThreeCardMatch Just, input rank matches pending rank
      it "creates a FullHouseMatch from PendingMatches ThreeOfAKind (Just PendingMatch) if input card rank matches PendingMatch " $
        PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) (Just (PendingMatch Seven)) .:.  Seven `shouldBe` PendingMatches (FullHouseMatch (ThreeOfAKindRank Two) (PairRank Seven)) Nothing
      -- ThreeCardMatch Just, input rank doesn't match pending rank
      it "sets Maybe PendingMatch to Nothing for  FullHouseMatch and Just PendingMatch if input rank doesn't match pending" $
        PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) (Just (PendingMatch Seven)) .:.  Eight `shouldBe` PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) Nothing

      -- FullHouseMatch Nothing, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches (FullHouseMatch _ _) _ (Nothing)" $
        PendingMatches (FullHouseMatch (ThreeOfAKindRank Three) (PairRank Eight)) Nothing .:.  Ace `shouldBe`  PendingMatches (FullHouseMatch (ThreeOfAKindRank Three) (PairRank Eight)) Nothing
      -- FullHouseMatch Just, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches (FullHouseMatch _ _) _ (Just)" $
        PendingMatches (FullHouseMatch (ThreeOfAKindRank Three) (PairRank Eight)) (Just (PendingMatch Seven)) .:.  Ace `shouldBe`  PendingMatches (FullHouseMatch (ThreeOfAKindRank Three) (PairRank Eight)) Nothing

      -- FourCardMatch Nothing, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches  (FourCardMatch _) _ (Nothing)" $
        PendingMatches (FourCardMatch (FourOfAKindRank Three)) Nothing .:.  Eight `shouldBe`  PendingMatches (FourCardMatch (FourOfAKindRank Three)) Nothing
      -- FourOfAkIndMatch Just, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches  (FourCardMatch _) _ (Just)" $
        PendingMatches (FourCardMatch (FourOfAKindRank Three)) (Just (PendingMatch Eight)) .:.  Eight `shouldBe`  PendingMatches (FourCardMatch (FourOfAKindRank Three)) Nothing
    describe "buildHS (builds HandState from existing handstate (can be empty or 'NewHandState') and a card" $ do
      it "creates initial HandState from NewHandState and single card" $
        buildHS NewHandState (Card (Hearts, Eight))
        `shouldBe`
        HandState { couldBeFlush = True
                  , couldBeStraight = True
                  , currentHighestRank = Eight
                  , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                  , lastSeen = Card (Hearts, Eight)
                  }
      it "updates existing HandState with a new card: same suit, non adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank = Eight
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Hearts, Jack))
          `shouldBe`
          HandState { couldBeFlush = True
                    , couldBeStraight = False
                    , currentHighestRank = Jack
                    , matches = PendingMatches NoMatches (Just (PendingMatch Jack))
                    , lastSeen = Card (Hearts, Jack)
                    }
      it "updates existing HandState with a new card: same suit, adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank = Eight
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Hearts, Nine))
          `shouldBe`
          HandState { couldBeFlush = True
                    , couldBeStraight = True
                    , currentHighestRank = Nine
                    , matches = PendingMatches NoMatches (Just (PendingMatch Nine))
                    , lastSeen = Card (Hearts, Nine)
                    }
      it "updates existing HandState with a new card: different suit, adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank = Eight
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Nine))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = True
                    , currentHighestRank = Nine
                    , matches = PendingMatches NoMatches (Just (PendingMatch Nine))
                    , lastSeen = Card (Clubs, Nine)
                    }
      it "updates existing HandState with a new card: different suit, non-adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank = Eight
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Ten))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = False
                    , currentHighestRank = Ten
                    , matches = PendingMatches NoMatches (Just (PendingMatch Ten))
                    , lastSeen = Card (Clubs, Ten)
                    }
      it "updates existing HandState with a new card: pair " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank = Eight
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Eight))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = False
                    , currentHighestRank = Eight
                    , matches = PendingMatches (PairMatch (PairRank Eight)) Nothing
                    , lastSeen = Card (Clubs, Eight)
                    }
