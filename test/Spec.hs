import           Data.Cards
import           Data.Hand
import           Test.Hspec

royalFlushCards = [Card (Clubs, Jack), Card (Clubs, Ten), Card (Clubs, Ace), Card (Clubs, Queen), Card (Clubs, King)]
sortedRoyalFlushCards = [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, King), Card (Clubs, Ace)]
royalFlushIntermediateState = HandState {couldBeFlush = True, couldBeStraight = True, currentHighCard = Card (Clubs,Ace), matches = PendingMatches NoMatches Nothing, lastSeen = Card(Clubs,Ace)}
royalFlushHandState = FinalHandState IsFlush IsStraight NoMatches (HighCard (Card (Clubs, Ace)))


straightFlushCards = [Card (Hearts, Three), Card (Hearts, Five), Card (Hearts, Six), Card (Hearts, Four), Card (Hearts, Two) ]
sortedStraightFlushCards = [Card (Hearts, Two), Card (Hearts, Three), Card (Hearts, Four), Card (Hearts, Five), Card (Hearts, Six)]
straightFlushIntermediateHandState = HandState {couldBeFlush = True, couldBeStraight = True, currentHighCard = Card (Clubs, Nine), matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
straightFlushHandState = FinalHandState IsFlush IsStraight NoMatches (HighCard (Card (Clubs, Nine)))

straightIntermediateHandState = HandState {couldBeFlush = False, couldBeStraight = True, currentHighCard = Card (Clubs, Nine), matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
straightHandState = FinalHandState IsNotFlush IsStraight NoMatches (HighCard (Card (Clubs, Nine)))

flushHandIntermediateHandState = HandState {couldBeFlush = True, couldBeStraight = False, currentHighCard = Card (Clubs, Nine), matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
flushHandState = FinalHandState IsFlush IsNotStraight NoMatches (HighCard (Card (Clubs, Nine)))

pairHandState = FinalHandState IsNotFlush IsNotStraight (PairMatch (PairRank Three)) (HighCard (Card (Hearts, Queen)))

twoPairHandState = FinalHandState IsNotFlush IsNotStraight (TwoPairMatch (PairRank Three) (PairRank Six)) (HighCard (Card (Hearts, Queen)))

threeOfAKindHandState = FinalHandState IsNotFlush IsNotStraight (ThreeCardMatch (ThreeOfAKindRank Eight)) (HighCard (Card (Hearts, Queen)))

fourOfAKindHandState = FinalHandState IsNotFlush IsNotStraight (FourOfAKindMatch Eight) (HighCard (Card (Hearts, Queen)))

fullHouseHandState = FinalHandState IsNotFlush IsNotStraight (FullHouseMatch (ThreeOfAKindRank Eight) (PairRank Ace)) (HighCard (Card (Hearts, Ace)))



main :: IO ()
main = hspec $ do
  describe "Data.Hand" $ do
    describe "playerCards" $ do
      it "converts list of cards to Just sorted 5 player cards when cards are list of 5 unique card values" $
        playerCards straightFlushCards `shouldBe` Just (PlayerCards straightFlushCards)
      it "converts list of cards to Nothing  when cards are not of length 5 card values" $
        playerCards (Card (Hearts, King): straightFlushCards) `shouldBe` Nothing
      it "converts list of cards to Nothing  when cards are length 5, but not unique card values" $
        playerCards [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, Ace), Card (Clubs, Ace)]  `shouldBe` Nothing
    describe "sortPlayerCards" $ do
      it "sorts playerCards in ascending order" $
        sortPlayerCards <$> playerCards royalFlushCards `shouldBe` Just (PlayerCards sortedRoyalFlushCards)
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
        PendingMatches (PairMatch (PairRank Two)) (Just (PendingMatch Three)) .:.  Three `shouldBe` PendingMatches (TwoPairMatch (PairRank Two) (PairRank Three)) Nothing

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
        PendingMatches (ThreeCardMatch (ThreeOfAKindRank Two)) Nothing .:.  Two `shouldBe` PendingMatches (FourOfAKindMatch Two) Nothing
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

      -- FourOfAKindMatch Nothing, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches  (FourOfAKindMatch _) _ (Nothing)" $
        PendingMatches (FourOfAKindMatch Three) Nothing .:.  Eight `shouldBe`  PendingMatches (FourOfAKindMatch Three) Nothing
      -- FourOfAkIndMatch Just, any input rank
      it "always returns existing PendingMatches if existing is PendingMatches  (FourOfAKindMatch _) _ (Just)" $
        PendingMatches (FourOfAKindMatch Three) (Just (PendingMatch Eight)) .:.  Eight `shouldBe`  PendingMatches (FourOfAKindMatch Three) Nothing

    describe "buildHS (builds HandState from existing handstate (can be empty or 'NewHandState') and a card" $ do
      it "creates initial HandState from NewHandState and single card" $
        buildHS NewHandState (Card (Hearts, Eight))
        `shouldBe`
        HandState { couldBeFlush = True
                  , couldBeStraight = True
                  , currentHighCard = Card (Hearts,Eight)
                  , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                  , lastSeen = Card (Hearts, Eight)
                  }
      it "updates existing HandState with a new card: same suit, non adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = Card (Hearts, Eight)
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Hearts, Jack))
          `shouldBe`
          HandState { couldBeFlush = True
                    , couldBeStraight = False
                    , currentHighCard = Card (Hearts, Jack)
                    , matches = PendingMatches NoMatches (Just (PendingMatch Jack))
                    , lastSeen = Card (Hearts, Jack)
                    }
      it "updates existing HandState with a new card: same suit, adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = Card (Hearts, Eight)
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Hearts, Nine))
          `shouldBe`
          HandState { couldBeFlush = True
                    , couldBeStraight = True
                    , currentHighCard = Card (Hearts, Nine)
                    , matches = PendingMatches NoMatches (Just (PendingMatch Nine))
                    , lastSeen = Card (Hearts, Nine)
                    }
      it "updates existing HandState with a new card: different suit, adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = Card (Hearts, Eight)
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Nine))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = True
                    , currentHighCard = Card (Clubs, Nine)
                    , matches = PendingMatches NoMatches (Just (PendingMatch Nine))
                    , lastSeen = Card (Clubs, Nine)
                    }
      it "updates existing HandState with a new card: different suit, non-adjacent rank " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = Card (Hearts, Eight)
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Ten))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = False
                    , currentHighCard = Card (Clubs, Ten)
                    , matches = PendingMatches NoMatches (Just (PendingMatch Ten))
                    , lastSeen = Card (Clubs, Ten)
                    }
      it "updates existing HandState with a new card: pair " $
        buildHS HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = Card (Hearts, Eight)
                          , matches = PendingMatches NoMatches (Just (PendingMatch Eight))
                          , lastSeen = Card (Hearts, Eight)
                          }
                (Card (Clubs, Eight))
          `shouldBe`
          HandState { couldBeFlush = False
                    , couldBeStraight = False
                    , currentHighCard = Card (Clubs, Eight)
                    , matches = PendingMatches (PairMatch (PairRank Eight)) Nothing
                    , lastSeen = Card (Clubs, Eight)
                    }
    describe "getHandState (building handstate from a sorted, unique group of 5 cards)" $ do
       it "gets royalflush HandState" $
        getHandState <$> (sortPlayerCards <$> playerCards royalFlushCards) `shouldBe`  Just royalFlushHandState
    describe "isRoyalFlush" $ do
      it "returns True when FinalHandState indicates RoyalFlush" $
        isRoyalFlush royalFlushHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate RoyalFlush" $
        isRoyalFlush straightFlushHandState `shouldBe` False
    describe "isStraightFlush" $ do
      it "returns True when FinalHandState indicates StraightFlush" $
        isStraightFlush straightFlushHandState `shouldBe` True
      it "returns False when HandState doesn't indicate StraightFlush" $
        isStraightFlush royalFlushHandState `shouldBe` False
    describe "isStraight" $ do
      it "returns True when FinalHandState indicates Straight" $
        isStraight straightHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate ONLY Straight" $
        isStraight royalFlushHandState `shouldBe` False
    describe "isFlush" $ do
      it "returns True when FinalHandState indicates Flush" $
        isFlush flushHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate Straight" $
        isFlush straightFlushHandState `shouldBe` False
    describe "isPair" $ do
      it "returns True when FinalHandState indicates Pair" $
        isPair pairHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate Pair" $
        isPair straightHandState `shouldBe` False
    describe "isTwoPair" $ do
      it "returns True when FinalHandState indicates TwoPair" $
        isTwoPair twoPairHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate TwoPair" $
        isTwoPair straightHandState `shouldBe` False
    describe "isThreeOfAKind" $ do
      it "returns True when FinalHandState indicates ThreeOfAKind" $
        isThreeOfAKind threeOfAKindHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate ThreeOfAKind" $
        isThreeOfAKind straightHandState `shouldBe` False
    describe "isFourOfAKind" $ do
      it "returns True when FinalHandState indicates FourOfAKind" $
        isFourOfAKind fourOfAKindHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate FourOfAKind" $
        isFourOfAKind straightHandState `shouldBe` False
    describe "isFullHouse" $ do
      it "returns True when FinalHandState indicates FullHouse" $
        isFullHouse fullHouseHandState `shouldBe` True
      it "returns False when FinalHandState doesn't indicate FullHouse" $
        isFullHouse straightHandState `shouldBe` False
    -- describe "Data.Hand (getHand)" $  dostraightFlushHandState = FinalHandState IsFlush IsStraight NoMatches HighCard (Card (Clubs, King))
        -- describe "findKicker" $ do
        --     it "sets kicker as max when both are equal to max" $
        --         findKicker (HighCard (Card (Clubs, Ace))) (Kicker (Card (Clubs, Ace))) (Kicker (Card (Hearts, Ace))) `shouldBe` Kicker (Card (Clubs, Ace))
        --     it "sets kicker as the lower card when first possible kicker is equal to max" $
        --         findKicker (HighCard (Card (Clubs, Ace))) (Kicker (Card (Clubs, Ace))) (Kicker (Card (Hearts, Jack))) `shouldBe` Kicker (Card (Hearts, Jack))
        --     it "sets kicker as the lower card when second possible kicker is equal to max" $
        --         findKicker (HighCard (Card (Clubs, Ace))) (Kicker (Card (Hearts, Jack))) (Kicker (Card (Hearts, Ace))) `shouldBe` Kicker (Card (Hearts, Jack))
        --     it "sets kicker as the first of the possible kickers when first is greater than second but both are less than max" $
        --         findKicker (HighCard (Card (Clubs, Ace))) (Kicker (Card (Hearts, Jack))) (Kicker (Card (Diamonds, Nine))) `shouldBe` Kicker (Card (Hearts, Jack))
        --     it "sets kicker as the second of the possible kickers when second is greater than first but both are less than max" $
        --         findKicker (HighCard (Card (Clubs, Ace))) (Kicker (Card (Spades, Eight))) (Kicker (Card (Hearts, Jack))) `shouldBe` Kicker (Card (Hearts, Jack))

