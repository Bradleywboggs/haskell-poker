module Data.Hand where

import           Data.Cards            (Card (..), Rank (..), Suit (..),
                                        getCardRank, isAdjacentRank)
import qualified Data.List             as L
import           System.Random
import qualified System.Random.Shuffle as S

data Shuffled
data Unshuffled
newtype Deck a = Deck [Card] deriving Show

ranks :: [Rank]
ranks = [Two .. Ace]

suits :: [Suit]
suits = [Spades, Clubs, Hearts, Diamonds]

deck :: Deck Unshuffled
deck = Deck $ [ Card (suit, rank)
              | rank <- ranks
              , suit <- suits
              ]

shuffle :: Deck a -> Deck Shuffled
shuffle (Deck cards) = Deck $ S.shuffle' cards 52 (mkStdGen 52)

deal :: Deck Shuffled -> [Card]
deal (Deck cards) = take 5 cards

data Sorted5Cards
data Unsorted5Cards
newtype PlayerCards a = PlayerCards [Card] deriving (Show, Eq)

newtype HighCardRank     = HighCardRank Rank deriving (Show, Eq, Ord)
newtype Kicker           = Kicker Rank deriving (Show, Eq, Ord)
newtype ThreeOfAKindRank = ThreeOfAKindRank Rank  deriving (Eq, Show, Ord)
newtype PairRank         = PairRank Rank  deriving (Eq, Show, Ord)
newtype FourOfAKindRank  = FourOfAKindRank Rank deriving (Eq, Show, Ord)
newtype PendingMatch     = PendingMatch Rank deriving (Show, Eq)


data MatchingCards = PairMatch PairRank
                    | TwoPairMatch PairRank PairRank
                    | ThreeCardMatch ThreeOfAKindRank
                    | FullHouseMatch ThreeOfAKindRank PairRank
                    | FourCardMatch FourOfAKindRank
                    | NoMatches
                    deriving (Show, Eq)

data Hand = HighCard HighCardRank Kicker (PlayerCards Sorted5Cards)
          | Pair PairRank HighCardRank Kicker (PlayerCards Sorted5Cards)
          | TwoPair PairRank PairRank HighCardRank (PlayerCards Sorted5Cards)
          | ThreeOfAKind ThreeOfAKindRank HighCardRank Kicker (PlayerCards Sorted5Cards)
          | Straight HighCardRank (PlayerCards Sorted5Cards)
          | Flush HighCardRank (PlayerCards Sorted5Cards)
          | FullHouse ThreeOfAKindRank PairRank (PlayerCards Sorted5Cards)
          | FourOfAKind FourOfAKindRank HighCardRank (PlayerCards Sorted5Cards)
          | StraightFlush HighCardRank (PlayerCards Sorted5Cards)
          | RoyalFlush (PlayerCards Sorted5Cards)
        deriving (Show)

data FlushState = IsFlush | IsNotFlush deriving (Show, Eq)
data StraightState = IsStraight | IsNotStraight deriving (Show, Eq)
data HandState = HandState
                   { couldBeFlush       :: !Bool
                   , couldBeStraight    :: !Bool
                   , currentHighestRank :: !Rank
                   , matches            :: PendingMatches
                   , lastSeen           :: !Card
                   }
               | NewHandState
  deriving (Show, Eq)

data FinalHandState = FinalHandState FlushState StraightState MatchingCards HighCardRank deriving (Show, Eq)
data PendingMatches = PendingMatches MatchingCards (Maybe PendingMatch) deriving (Show, Eq)

initialMatch :: PendingMatches
initialMatch = PendingMatches NoMatches Nothing

unwrapMatches :: PendingMatches -> MatchingCards
unwrapMatches (PendingMatches match _) = match

maximumWhere :: Ord a => (a -> Bool) -> [a] -> a
maximumWhere f xs = maximum $ filter f xs

infix 6 .:.
-- cons operator for PendingMatches + Card
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

PendingMatches twoPair@(TwoPairMatch (PairRank pair1Rank) (PairRank pair2Rank)) Nothing .:. rank =
    if pair2Rank == rank then PendingMatches (FullHouseMatch (ThreeOfAKindRank pair2Rank) (PairRank pair1Rank)) Nothing else PendingMatches twoPair Nothing

PendingMatches twoPair@(TwoPairMatch _ _) _ .:. _ =  PendingMatches twoPair Nothing

PendingMatches pair@(PairMatch pairRank) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (TwoPairMatch pairRank (PairRank rank)) Nothing
        else PendingMatches pair (Just (PendingMatch rank))

PendingMatches triad@(ThreeCardMatch triadRank) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (FullHouseMatch triadRank (PairRank rank)) Nothing
        else PendingMatches triad Nothing

PendingMatches four@(FourCardMatch _) _ .:. _ = PendingMatches four Nothing
PendingMatches fullHouse@(FullHouseMatch _ _) _ .:. _ = PendingMatches fullHouse Nothing

playerCards :: [Card] -> Maybe (PlayerCards Unsorted5Cards)
playerCards cards = if (length . L.nub) cards /= 5
                        then Nothing
                        else Just (PlayerCards cards)

sortPlayerCards :: PlayerCards a -> PlayerCards Sorted5Cards
sortPlayerCards (PlayerCards cards) = PlayerCards $ L.sort cards

hand :: PlayerCards Sorted5Cards -> Hand
hand cards = determineHand (getHandState cards) cards

determineHand :: FinalHandState -> PlayerCards Sorted5Cards -> Hand
determineHand (FinalHandState IsFlush IsStraight NoMatches (HighCardRank Ace)) cards = RoyalFlush cards
determineHand (FinalHandState IsFlush IsStraight NoMatches highcardRank) cards = StraightFlush highcardRank cards
determineHand (FinalHandState IsFlush IsNotStraight NoMatches highcardRank) cards = Flush highcardRank cards
determineHand (FinalHandState IsNotFlush IsStraight NoMatches highcardRank) cards = Straight highcardRank cards
determineHand (FinalHandState _ _ (FullHouseMatch  three two) _) cards = FullHouse three two cards
determineHand (FinalHandState _ _ (FourCardMatch (FourOfAKindRank matchRank)) _) cards@(PlayerCards cs) =
        FourOfAKind (FourOfAKindRank matchRank) (HighCardRank highcardRank) cards
                where highcardRank = maximumWhere (/= matchRank) (getCardRank <$> cs)
determineHand (FinalHandState _ _ (TwoPairMatch (PairRank matchRank) (PairRank matchRank')) _) cards@(PlayerCards cs) =
        TwoPair (PairRank matchRank) (PairRank matchRank') (HighCardRank highcardRank) cards
          where highcardRank = maximumWhere (`notElem` [matchRank, matchRank']) (getCardRank <$> cs)
determineHand (FinalHandState _ _ (ThreeCardMatch three@(ThreeOfAKindRank matchRank)) _) cards@(PlayerCards cs) =
         ThreeOfAKind three (HighCardRank highcardRank) (Kicker kicker) cards
           where
                cardRanks    = getCardRank <$> cs
                highcardRank = maximumWhere (/= matchRank) cardRanks
                kicker       = maximumWhere (`notElem` [highcardRank, matchRank]) cardRanks


determineHand (FinalHandState _ _ (PairMatch two@(PairRank matchRank)) _) cards@(PlayerCards cs) =
        Pair two (HighCardRank highcardRank) (Kicker kicker) cards
        where
            cardRanks    = getCardRank <$> cs
            highcardRank = maximumWhere (/= matchRank) cardRanks
            kicker       = maximumWhere (`notElem` [matchRank, highcardRank]) cardRanks

determineHand (FinalHandState _ _ _ highcard) cards@(PlayerCards cs) = HighCard highcard kicker cards
        where kicker = Kicker $ (getCardRank <$> cs) !! 3

finalizeHandState :: HandState -> FinalHandState
finalizeHandState hs = FinalHandState flushState straightState matchesState highcard
                        where
                            flushState = if couldBeFlush hs then IsFlush else IsNotFlush
                            straightState = if couldBeStraight hs then IsStraight else IsNotStraight
                            matchesState = unwrapMatches (matches hs)
                            highcard = HighCardRank $ currentHighestRank hs

getHandState :: PlayerCards Sorted5Cards -> FinalHandState
getHandState (PlayerCards cards) = finalizeHandState $ L.foldl' buildHS NewHandState cards


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



instance Eq Hand where
    (==) (HighCard highcardRank kicker _) (HighCard highcardRank' kicker' _) =
            (highcardRank, kicker)  == (highcardRank', kicker')
    (==) (Pair pair highcardRank kicker _) (Pair pair' highcardRank' kicker' _)   =
            (pair, highcardRank, kicker)  == (pair', highcardRank', kicker')
    (==) (TwoPair pairRank1 pairRank1' highcardRank _) (TwoPair pairRank2 pairRank2' highcardRank' _) =
            (pairRank1, pairRank1', highcardRank)  == (pairRank2, pairRank2', highcardRank')
    (==) (ThreeOfAKind triad highcardRank kicker _) (ThreeOfAKind triad' highcardRank' kicker' _) =
            (triad, highcardRank, kicker)     == (triad', highcardRank', kicker')
    (==) (Straight highcardRank _) (Straight highcardRank' _) =
            highcardRank == highcardRank'
    (==) (Flush highcardRank _) (Flush highcardRank' _) =
            highcardRank == highcardRank'
    (==) (FullHouse threeRank twoRank _) (FullHouse threeRank' twoRank' _) =
            (threeRank, twoRank) == (threeRank', twoRank')
    (==) (FourOfAKind quad kicker _) (FourOfAKind quad' kicker' _)   =
            (quad, kicker)      == (quad', kicker')
    (==) (StraightFlush highcardRank _) (StraightFlush highcardRank' _) =
           highcardRank == highcardRank'
    (==) (RoyalFlush _) (RoyalFlush _) = True
    (==) _ _                                                                 = False

instance Ord Hand where
    compare (RoyalFlush _ )(RoyalFlush  _) = EQ
    compare (RoyalFlush  _) _             = GT
    compare _ (RoyalFlush _) = LT
    compare (StraightFlush highcardRank _) (StraightFlush highcardRank' _) = compare highcardRank highcardRank'
    compare (StraightFlush _ _) _ = GT
    compare _ (StraightFlush _ _) = LT
    compare (FourOfAKind quad highcardRank _) (FourOfAKind quad' highcardRank' _) = compare (quad, highcardRank) (quad', highcardRank')
    compare FourOfAKind{} _ = GT
    compare _ FourOfAKind{} = LT
    compare (FullHouse threeRank twoRank _) (FullHouse threeRank' twoRank' _) = compare (threeRank, twoRank) (threeRank', twoRank')
    compare FullHouse{} _ = GT
    compare _ FullHouse{} = LT
    compare (Flush highcardRank _) (Flush highcardRank' _) = compare highcardRank highcardRank'
    compare (Flush _ _) _ = GT
    compare _ (Flush _ _) = LT
    compare (Straight highcardRank _) (Straight highcardRank' _) = compare highcardRank highcardRank'
    compare (Straight _ _) _ = GT
    compare _ (Straight _ _) = LT
    compare (ThreeOfAKind triad highcardRank kicker _ ) (ThreeOfAKind triad' highcardRank' kicker' _) = compare (triad, highcardRank, kicker) (triad', highcardRank', kicker')
    compare ThreeOfAKind{} _ = GT
    compare _ ThreeOfAKind{} = LT
    compare (TwoPair twoP1 twoP1' highcardRank _) (TwoPair twoP2 twoP2' highcardRank' _) = compare (twoP1, twoP1', highcardRank) (twoP2, twoP2', highcardRank')
    compare TwoPair{} _ = GT
    compare _ TwoPair{} = LT
    compare (Pair pair highcardRank kicker _) (Pair pair' highcardRank' kicker' _) = compare (pair, highcardRank, kicker) (pair', highcardRank', kicker')
    compare Pair{} _ = GT
    compare _ Pair{}  = LT
    compare (HighCard highcardRank kicker _) (HighCard highcardRank' kicker' _) = compare (highcardRank, kicker) (highcardRank', kicker')
