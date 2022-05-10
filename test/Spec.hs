import           Data.Cards
import           Data.PokerHand
import           Data.PokerHand.Builder
import           Data.Maybe
import           Test.Hspec


defaultHighCardHand = PlayerCards[Card(Hearts, Two), Card(Clubs, Three), Card(Diamonds, Four), Card(Spades, Five), Card(Hearts, Seven)]
royalFlushCards = [Card (Clubs, Jack), Card (Clubs, Ten), Card (Clubs, Ace), Card (Clubs, Queen), Card (Clubs, King)]
sortedRoyalFlushCards = [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, King), Card (Clubs, Ace)]
royalFlushIntermediateState = HandState {couldBeFlush = True, couldBeStraight = True, currentHighestRank = Two, matches = PendingMatches NoMatches Nothing, lastSeen = Card(Clubs,Ace)}
royalFlushHandState = FinalHandState IsFlush IsStraight NoMatches (HighCardRank  Ace)


straightFlushCards = [Card (Hearts, Three), Card (Hearts, Five), Card (Hearts, Six), Card (Hearts, Four), Card (Hearts, Two) ]
sortedStraightFlushCards = [Card (Hearts, Two), Card (Hearts, Three), Card (Hearts, Four), Card (Hearts, Five), Card (Hearts, Six)]
straightFlushIntermediateHandState = HandState {couldBeFlush = True, couldBeStraight = True, currentHighestRank = Nine, matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
straightFlushHandState = FinalHandState IsFlush IsStraight NoMatches (HighCardRank  Nine)

straightIntermediateHandState = HandState {couldBeFlush = False, couldBeStraight = True, currentHighestRank = Nine, matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
straightHandState = FinalHandState IsNotFlush IsStraight NoMatches (HighCardRank  Nine)
straightCards = [Card (Clubs, Jack), Card (Hearts, Ten), Card (Clubs, Nine), Card (Clubs, Queen), Card (Clubs, King)]

flushCards = [Card (Clubs, Seven), Card (Clubs, Two), Card (Clubs, Eight), Card (Clubs, Four), Card (Clubs, Nine)]
flushHandIntermediateHandState = HandState {couldBeFlush = True, couldBeStraight = False, currentHighestRank = Nine, matches = PendingMatches NoMatches (Just (PendingMatch Nine)) , lastSeen = Card (Clubs, Nine)}
flushHandState = FinalHandState IsFlush IsNotStraight NoMatches (HighCardRank  Nine)

pairCards = [Card (Clubs, Ten),  Card (Diamonds, Seven), Card (Hearts, Ten), Card (Spades, King), Card (Spades, Jack)]
pairHandState = FinalHandState IsNotFlush IsNotStraight (PairMatch (PairRank Three)) (HighCardRank Queen)

twoPairCards = [Card (Clubs, Ten), Card (Hearts, Jack), Card (Spades, King), Card (Hearts, Ten), Card (Diamonds, King)]
twoPairHandState = FinalHandState IsNotFlush IsNotStraight (TwoPairMatch (PairRank Three) (PairRank Six)) (HighCardRank Queen)

threeOfAKindCards = [Card (Clubs, Ten), Card (Hearts, Ten), Card (Spades, King), Card (Clubs, Jack), Card (Diamonds, Ten)]
threeOfAKindHandState = FinalHandState IsNotFlush IsNotStraight (ThreeCardMatch (ThreeOfAKindRank Eight)) (HighCardRank Queen)

fourofAKindCards = [Card (Clubs, Ten), Card (Hearts, Ten), Card (Spades, Ten), Card (Clubs, Seven), Card (Diamonds, Ten)]
fourOfAKindHandState = FinalHandState IsNotFlush IsNotStraight (FourCardMatch (FourOfAKindRank Eight)) (HighCardRank Queen)

fullHouseHandStateLowsOfHighs = FinalHandState IsNotFlush IsNotStraight (FullHouseMatch (ThreeOfAKindRank Eight) (PairRank Ace)) (HighCardRank Ace)
fullHouseCardsLowsOfHighs = [Card (Clubs, Ten), Card (Hearts, Ten), Card (Hearts, King), Card (Clubs, King), Card (Diamonds, Ten)]

fullHouseHandStateHighsOfLows = FinalHandState IsNotFlush IsNotStraight (FullHouseMatch (ThreeOfAKindRank Ace) (PairRank Eight)) (HighCardRank Ace)
fullHouseCardsHighsOfLows = [Card (Clubs, Ten), Card (Diamonds, King), Card (Hearts, King), Card (Clubs, King), Card (Diamonds, Ten)]

crapCards = [Card(Hearts, Two), Card(Clubs, Three), Card(Diamonds, Eight), Card(Spades, Five), Card(Hearts, Six)]



main :: IO ()
main = hspec $ do
  describe "Data.Hand" $ do
    describe "toPokerCards" $ do
      it "converts list of cards to Just sorted 5 player cards when cards are list of 5 unique card values" $
        toPokerCards straightFlushCards `shouldBe` Just (PlayerCards straightFlushCards)
      it "converts list of cards to Nothing  when cards are not of length 5 card values" $
        toPokerCards (Card (Hearts, King): straightFlushCards) `shouldBe` Nothing
      it "converts list of cards to Nothing  when cards are length 5, but not unique card values" $
        toPokerCards [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, Ace), Card (Clubs, Ace)]  `shouldBe` Nothing
    describe "sortPokerCards" $ do
      it "sorts toPokerCards in ascending order" $
        sortPokerCards <$> toPokerCards royalFlushCards `shouldBe` Just (PlayerCards sortedRoyalFlushCards)
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
    describe "toHand (creating Poker Hand)" $ do
       it "returns RoyalFlush with royal flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards royalFlushCards) in
          toHand cards `shouldBe` RoyalFlush (PlayerCards sortedRoyalFlushCards)
       it "returns StraightFlush with straight flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards straightFlushCards) in
          toHand cards `shouldBe` StraightFlush (HighCardRank Six) cards
       it "returns FourOfAKind with four of a kind cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fourofAKindCards) in
          toHand cards `shouldBe` FourOfAKind (FourOfAKindRank Ten) (HighCardRank Seven) cards
       it "returns Flush with flush cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards flushCards) in
          toHand cards `shouldBe` Flush (HighCardRank Nine) cards
       it "returns Straight with straight cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards straightCards) in
          toHand cards `shouldBe` Straight (HighCardRank King) cards
       it "returns FullHouse with fullhouse cards ( lows of high ) " $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fullHouseCardsLowsOfHighs) in
          toHand cards `shouldBe` FullHouse (ThreeOfAKindRank Ten) (PairRank King) cards
       it "returns FullHouse with fullhouse cards ( highs  of low ) " $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fullHouseCardsHighsOfLows) in
          toHand cards `shouldBe` FullHouse (ThreeOfAKindRank King) (PairRank Ten) cards
       it "returns ThreeOfAKind with three of a kind cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards threeOfAKindCards) in
          toHand cards `shouldBe` ThreeOfAKind (ThreeOfAKindRank Ten) (HighCardRank King) (Kicker Jack) cards
       it "returns Pair with pair cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards pairCards) in
          toHand cards `shouldBe` Pair (PairRank Ten) (HighCardRank King) (Kicker Jack) cards
       it "returns HighCard with crap cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards crapCards) in
          toHand cards `shouldBe` HighCard (HighCardRank Eight) (Kicker Six) cards
       it "returns TwoPair with three of a two pair cards" $
        let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards twoPairCards) in
          toHand cards `shouldBe` TwoPair (PairRank King) (PairRank Ten) (HighCardRank Jack) cards
    describe "simple hand comparisons (>, <)" $ do
      describe "RoyalFlush" $ do
        it "beats StraightFlush" $
          let
            rfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards royalFlushCards)
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards straightFlushCards)
          in
          RoyalFlush rfcards > StraightFlush (HighCardRank Six) sfcards `shouldBe` True
      describe "StraightFlush" $ do
        it "loses to RoyalFlush" $
          let
            rfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards royalFlushCards)
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards straightFlushCards)
          in
          StraightFlush (HighCardRank Six) sfcards < RoyalFlush rfcards `shouldBe` True
        it "beats FourOfAKind" $
          let
            sfcards = maybe defaultHighCardHand sortPokerCards (toPokerCards straightFlushCards)
            foak = maybe defaultHighCardHand sortPokerCards (toPokerCards fourofAKindCards)
          in
          StraightFlush (HighCardRank Six) sfcards > FourOfAKind (FourOfAKindRank Ten) (HighCardRank Seven) foak `shouldBe` True
    describe "show Hand" $ do
      describe "show FullHouse" $ do
        it "displays FullHouse, low full of high" $ 
            let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fullHouseCardsLowsOfHighs) in
            show (toHand cards) `shouldBe` " [ 10\9827\65039 ] [ 10\9829\65039 ] [ 10\9830\65039 ] [ K\9829\65039 ] [ K\9827\65039 ]\nFull House, 10's full of K's"
        -- it "displays FullHouse, low full of high" $ 
        --     let cards = maybe defaultHighCardHand sortPokerCards (toPokerCards fullHouseCardsLowsOfHighs) in
        --     show (hand cards) `shouldBe` ""
          
    -- describe "same hand comparisons (>, <)" $ do
    --   it "2 TwoPair hands compares highest pair first" $
    --     let gtTwoPairCards = maybe defaultHighCardHand sortPlayer

