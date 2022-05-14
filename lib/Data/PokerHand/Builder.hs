module Data.PokerHand.Builder where

import           Data.Cards (Card (..), Rank (..), isAdjacentRank)

data HandState = HandState
                   { couldBeFlush       :: !Bool
                   , couldBeStraight    :: !Bool
                   , currentHighestRank :: !Rank
                   , matches            :: PendingMatches
                   , lastSeen           :: !Card
                   }
               | NewHandState
  deriving (Show, Eq)

newtype HighCardRank     = HighCardRank Rank deriving (Eq, Ord)
instance Show HighCardRank where
    show (HighCardRank r) = "High Card: " ++ show r

newtype Kicker           = Kicker Rank deriving (Eq, Ord)
instance Show Kicker where
    show (Kicker r) = "Kicker: " ++ show r

newtype ThreeOfAKindRank = ThreeOfAKindRank Rank  deriving (Eq,  Ord)
instance Show ThreeOfAKindRank where
    show (ThreeOfAKindRank r) = "Three " ++ show r ++ "'s"

newtype PairRank         = PairRank Rank  deriving (Eq, Ord)
instance Show PairRank where
    show (PairRank r) = "Pair of " ++ show r ++ "'s"

newtype FourOfAKindRank  = FourOfAKindRank Rank deriving (Eq, Ord)
instance Show FourOfAKindRank where
    show (FourOfAKindRank r) = "Four " ++ show r ++ "'s"

newtype PendingMatch     = PendingMatch Rank deriving (Show, Ord, Eq)


data MatchingCards = PairMatch PairRank
                    | TwoPairMatch PairRank PairRank
                    | ThreeCardMatch ThreeOfAKindRank
                    | FullHouseMatch ThreeOfAKindRank PairRank
                    | FourCardMatch FourOfAKindRank
                    | NoMatches
                    deriving (Show, Eq)

data PendingMatches      = PendingMatches MatchingCards (Maybe PendingMatch) deriving (Show, Eq)

data FlushState          = IsFlush | IsNotFlush deriving (Show, Eq)
data StraightState       = IsStraight | IsNotStraight deriving (Show, Eq)
data FinalHandState      = FinalHandState FlushState StraightState MatchingCards HighCardRank deriving (Show, Eq)


finalizeHandState :: HandState -> FinalHandState
finalizeHandState hs = FinalHandState flushState straightState matchesState highcard
                        where
                            flushState     = if couldBeFlush hs then IsFlush else IsNotFlush
                            straightState = if couldBeStraight hs then IsStraight else IsNotStraight
                            matchesState  = unwrapMatches (matches hs)
                            highcard = HighCardRank $ currentHighestRank hs


buildHS :: HandState -> Card -> HandState
buildHS NewHandState card@(Card (_, rank)) =
                HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighestRank  = rank
                          , matches = initialMatch .:. rank
                          , lastSeen = card
                          }

buildHS HandState { couldBeFlush = flush
                  , couldBeStraight = straight
                  , currentHighestRank  = currentHighest
                  , matches = currentPendingMatches
                  , lastSeen = Card (lastSeenSuit, lastSeenRank)
                  }
        currentCard@(Card (currentSuit, currentRank)) =
            HandState { couldBeFlush = flush && lastSeenSuit == currentSuit
                      , couldBeStraight = straight && isAdjacentRank lastSeenRank currentRank
                      , currentHighestRank  = max currentHighest currentRank
                      , matches =  newMatches
                      , lastSeen = currentCard
                      }
                    where
                        newMatches = currentPendingMatches .:. currentRank

initialMatch :: PendingMatches
initialMatch = PendingMatches NoMatches Nothing

unwrapMatches :: PendingMatches -> MatchingCards
unwrapMatches (PendingMatches match _) = match

infix 6 .:.
-- cons operator for PendingMatches + Rank
(.:.) :: PendingMatches -> Rank -> PendingMatches
PendingMatches NoMatches Nothing .:. rank = PendingMatches NoMatches (Just (PendingMatch rank))
PendingMatches NoMatches (Just (PendingMatch pendingMatch)) .:. rank =
        if pendingMatch == rank
            then PendingMatches (PairMatch (PairRank rank)) Nothing
            else PendingMatches NoMatches (Just (PendingMatch rank))

PendingMatches pair@(PairMatch (PairRank pairRank)) Nothing .:. rank =
        if  pairRank == rank then PendingMatches (ThreeCardMatch (ThreeOfAKindRank pairRank)) Nothing else PendingMatches pair (Just (PendingMatch rank))

PendingMatches triad@(ThreeCardMatch (ThreeOfAKindRank triadRank)) Nothing .:. rank =
    if triadRank == rank then PendingMatches (FourCardMatch (FourOfAKindRank rank)) Nothing else PendingMatches triad (Just (PendingMatch rank))

PendingMatches twoPair@(TwoPairMatch (PairRank pair1Rank) (PairRank pair2Rank)) Nothing .:. rank
    | pair1Rank == rank = PendingMatches (FullHouseMatch (ThreeOfAKindRank pair1Rank) (PairRank pair2Rank)) Nothing
    | pair2Rank == rank = PendingMatches (FullHouseMatch (ThreeOfAKindRank pair2Rank) (PairRank pair1Rank)) Nothing
    | otherwise         = PendingMatches twoPair Nothing

PendingMatches twoPair@(TwoPairMatch _ _) _ .:. _ =  PendingMatches twoPair Nothing

PendingMatches pair@(PairMatch (PairRank pairRank)) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (TwoPairMatch (PairRank (max pairRank rank )) (PairRank (min pairRank rank))) Nothing
        else PendingMatches pair (Just (PendingMatch rank))

PendingMatches triad@(ThreeCardMatch triadRank) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (FullHouseMatch triadRank (PairRank rank)) Nothing
        else PendingMatches triad Nothing

PendingMatches four@(FourCardMatch _) _ .:. _ = PendingMatches four Nothing
PendingMatches fullHouse@(FullHouseMatch _ _) _ .:. _ = PendingMatches fullHouse Nothing


