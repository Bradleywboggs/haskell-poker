module TestData where

import Data.PokerHand
import Data.PokerHand.Builder
import Data.Cards


defaultHighCardHand = PokerCards [Card(Hearts, Two), Card(Clubs, Three), Card(Diamonds, Four), Card(Spades, Five), Card(Hearts, Seven)]
royalFlushCards = [Card (Clubs, Jack), Card (Clubs, Ten), Card (Clubs, Ace), Card (Clubs, Queen), Card (Clubs, King)]
sortedRoyalFlushCards =  [Card (Clubs, Ten), Card (Clubs, Jack), Card (Clubs, Queen), Card (Clubs, King), Card (Clubs, Ace)]
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





