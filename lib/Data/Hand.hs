module Data.Hand where

import           Data.Cards (Card (..), Rank (..), Suit (..), isAdjacentRank)
import qualified Data.List  as L

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
shuffle = undefined

-- deal :: Deck Shuffled -> PlayerCount -> [[Card]]
-- deal = undefined

data Sorted5Cards
data Unsorted5Cards
newtype PlayerCards a = PlayerCards [Card] deriving (Show, Eq)

newtype HighCard = HighCard Card deriving (Show, Eq, Ord)
newtype Kicker = Kicker Card deriving (Show, Eq, Ord)

data Pair = Pair Rank Kicker deriving (Show, Eq, Ord)
data TwoPair = TwoPair Rank Kicker deriving (Show, Eq, Ord)
data ThreeOfAKind = ThreeOfAKind Rank Kicker deriving (Show, Eq, Ord)
newtype Straight = Straight HighCard deriving (Show, Eq, Ord)
newtype Flush = Flush HighCard deriving (Show, Eq, Ord)
data FullHouse = FullHouse Rank Rank deriving (Show, Eq, Ord)
data FourOfAKind = FourOfAKind Rank Kicker deriving (Show, Eq, Ord)
newtype StraightFlush = StraightFlush HighCard deriving (Show, Eq, Ord)
data RoyalFlush = RoyalFlush deriving (Show, Eq, Ord)

data Hand = HighCardHand HighCard Kicker (PlayerCards Sorted5Cards)
          | PairHand Pair (PlayerCards Sorted5Cards)
          | TwoPairHand TwoPair (PlayerCards Sorted5Cards)
          | ThreeOfAKindHand ThreeOfAKind (PlayerCards Sorted5Cards)
          | StraightHand Straight (PlayerCards Sorted5Cards)
          | FlushHand Flush (PlayerCards Sorted5Cards)
          | FullHouseHand FullHouse Kicker (PlayerCards Sorted5Cards)
          | FourOfAKindHand FourOfAKind (PlayerCards Sorted5Cards)
          | StraightFlushHand StraightFlush (PlayerCards Sorted5Cards)
          | RoyalFlushHand RoyalFlush (PlayerCards Sorted5Cards)
        deriving (Show)

data IntermediateState
data FinalState

data FlushState = IsFlush | IsNotFlush deriving (Show, Eq)
data StraightState = IsStraight | IsNotStraight deriving (Show, Eq)
data HandState = HandState { couldBeFlush    :: !Bool
                           , couldBeStraight :: !Bool
                           , currentHighCard :: !Card
                           , matches         :: PendingMatches
                           , lastSeen        :: !Card
                           }
                | NewHandState
                deriving (Show, Eq)
data FinalHandState = FinalHandState FlushState StraightState MatchingCards HighCard deriving (Show, Eq)
newtype ThreeOfAKindRank = ThreeOfAKindRank { getTriadRank :: Rank } deriving (Eq, Show, Ord)
newtype PairRank         = PairRank { getPairRank :: Rank } deriving (Eq, Show, Ord)
data MatchingCards = PairMatch PairRank
                    | TwoPairMatch PairRank PairRank
                    | ThreeCardMatch ThreeOfAKindRank
                    | FullHouseMatch ThreeOfAKindRank PairRank
                    | FourOfAKindMatch Rank
                    | NoMatches
                    deriving (Show, Eq)

newtype PendingMatch = PendingMatch {getPendingRank :: Rank} deriving (Show, Eq)
data PendingMatches = PendingMatches MatchingCards (Maybe PendingMatch) deriving (Show, Eq)

initialMatch :: PendingMatches
initialMatch = PendingMatches NoMatches Nothing

unwrapMatches :: PendingMatches -> MatchingCards
unwrapMatches (PendingMatches match _) = match


infix 6 .:.
-- cons operator for PendingMatches + Card
(.:.) :: PendingMatches -> Rank -> PendingMatches
PendingMatches NoMatches Nothing .:. rank = PendingMatches NoMatches (Just (PendingMatch rank))
PendingMatches NoMatches (Just pendingMatch) .:. rank =
        if getPendingRank pendingMatch == rank
            then PendingMatches (PairMatch (PairRank rank)) Nothing
            else PendingMatches NoMatches (Just (PendingMatch rank))

PendingMatches pair@(PairMatch pairRank) Nothing .:. rank =
        if getPairRank pairRank == rank then PendingMatches (ThreeCardMatch (ThreeOfAKindRank $ getPairRank pairRank)) Nothing else PendingMatches pair (Just (PendingMatch rank))

PendingMatches triad@(ThreeCardMatch triadRank) Nothing .:. rank =
    if getTriadRank triadRank == rank then PendingMatches (FourOfAKindMatch rank) Nothing else PendingMatches triad (Just (PendingMatch rank))

PendingMatches twoPair@(TwoPairMatch pair1Rank pair2Rank) Nothing .:. rank =
    if getPairRank pair2Rank == rank then PendingMatches (FullHouseMatch (ThreeOfAKindRank $ getPairRank pair2Rank) pair1Rank) Nothing else PendingMatches twoPair Nothing

PendingMatches twoPair@(TwoPairMatch _ _) _ .:. _ =  PendingMatches twoPair Nothing

PendingMatches pair@(PairMatch pairRank) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (TwoPairMatch pairRank (PairRank rank)) Nothing
        else PendingMatches pair (Just (PendingMatch rank))

PendingMatches triad@(ThreeCardMatch triadRank) (Just (PendingMatch singRank)) .:. rank =
    if singRank == rank
        then PendingMatches (FullHouseMatch triadRank (PairRank rank)) Nothing
        else PendingMatches triad Nothing

PendingMatches four@(FourOfAKindMatch _) _ .:. _ = PendingMatches four Nothing
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
determineHand hs cards
    | isRoyalFlush hs = RoyalFlushHand RoyalFlush cards
    | isStraightFlush hs = undefined --StraightFlushHand StraightFlush cards
    | isFourOfAKind hs = undefined
    | isFullHouse hs = undefined
    | isFlush hs = undefined
    | isStraight hs = undefined
    | isThreeOfAKind hs = undefined
    | isTwoPair hs = undefined
    | isPair hs = undefined
    | otherwise    = undefined -- HighCardHand


finalizeHandState :: HandState -> FinalHandState
finalizeHandState hs = FinalHandState flushState straightState matchesState highcard
                        where
                            flushState = if couldBeFlush hs then IsFlush else IsNotFlush
                            straightState = if couldBeStraight hs then IsStraight else IsNotStraight
                            matchesState = unwrapMatches (matches hs)
                            highcard = HighCard $ currentHighCard hs

getHandState :: PlayerCards Sorted5Cards -> FinalHandState
getHandState (PlayerCards cards) = finalizeHandState $ L.foldl' buildHS NewHandState cards


buildHS :: HandState -> Card -> HandState
buildHS NewHandState card@(Card (_, rank)) =
                HandState { couldBeFlush = True
                          , couldBeStraight = True
                          , currentHighCard = card
                          , matches = initialMatch .:. rank
                          , lastSeen = card
                          }

buildHS HandState { couldBeFlush = flush
                  , couldBeStraight = straight
                  , currentHighCard = highCard@(Card (_, currentHighestRank) )
                  , matches = currentPendingMatches
                  , lastSeen = Card (lastSeenSuit, lastSeenRank)
                  }
        currentCard@(Card (currentSuit, currentRank)) =
            HandState { couldBeFlush = flush && lastSeenSuit == currentSuit
                      , couldBeStraight = straight && isAdjacentRank lastSeenRank currentRank
                      , currentHighCard = if currentHighestRank > currentRank then highCard else currentCard
                      , matches =  currentPendingMatches .:. currentRank
                      , lastSeen = currentCard
                      }


-- instance Eq Hand where
--     (==) (HighCardHand highcard kicker _) (HighCardHand highcard' kicker' _) =
--             (highcard, kicker)  == (highcard', kicker')
--     (==) (PairHand pair kicker _) (PairHand pair' kicker' _)   =
--             (pair, kicker)      == (pair', kicker')
--     (==) (TwoPairHand twoPair kicker _) (TwoPairHand twoPair' kicker' _) =
--             (twoPair, kicker)   == (twoPair', kicker')
--     (==) (ThreeOfAKindHand triad kicker _) (ThreeOfAKindHand triad' kicker' _) =
--             (triad, kicker)     == (triad', kicker')
--     (==) (Straight highcard _) (Straight highcard' _) =
--             highcard == highcard'
--     (==) (Flush highcard _) (Flush highcard' _) =
--             highcard == highcard'
--     (==) (FullHouseHand fullhouse _) (FullHouseHand fullhouse' _) =
--             fullhouse == fullhouse'
--     (==) (FourOfAKindHand quad kicker _) (FourOfAKindHand quad' kicker' _)   =
--             (quad, kicker)      == (quad', kicker')
--     (==) (StraightFlush highcard _) (StraightFlush highcard' _) =
--            highcard == highcard'
--     (==) (RoyalFlush _) (RoyalFlush _) = True
--     (==) _ _                                                                 = False

-- instance Ord Hand where
--     compare (RoyalFlushHand _ _ )(RoyalFlushHand _ _) = EQ
--     compare (RoyalFlushHand _ _) _             = GT
--     compare _ (RoyalFlushHand _ _) = LT
--     compare (StraightFlush highcard _) (StraightFlush highcard' _) = compare highcard highcard'
--     compare (StraightFlush _ _) _ = GT
--     compare _ (StraightFlush _ _) = LT
--     compare (FourOfAKindHand quad kicker _) (FourOfAKindHand quad' kicker' _) = compare (quad, kicker) (quad', kicker')
--     compare FourOfAKindHand{} _ = GT
--     compare _ FourOfAKindHand{} = LT
--     compare (FullHouseHand fh _) (FullHouseHand fh' _) = compare fh fh'
--     compare FullHouseHand{} _ = GT
--     compare _ FullHouseHand{} = LT
--     compare (Flush highcard _) (Flush highcard' _) = compare highcard highcard'
--     compare (Flush _ _) _ = GT
--     compare _ (Flush _ _) = LT
--     compare (Straight highcard _) (Straight highcard' _) = compare highcard highcard'
--     compare (Straight _ _) _ = GT
--     compare _ (Straight _ _) = LT
--     compare (ThreeOfAKindHand triad kicker _ ) (ThreeOfAKindHand triad' kicker' _) = compare (triad, kicker) (triad', kicker')
--     compare ThreeOfAKindHand{} _ = GT
--     compare _ ThreeOfAKindHand{} = LT
--     compare (TwoPairHand twoP kicker _) (TwoPairHand twoP' kicker' _) = compare (twoP, kicker) (twoP', kicker')
--     compare TwoPairHand{} _ = GT
--     compare _ TwoPairHand{} = LT
--     compare (PairHand pair kicker _) (PairHand pair' kicker' _) = compare (pair, kicker) (pair', kicker')
--     compare PairHand{} _ = GT
--     compare _ PairHand{}  = LT
--     compare (HighCardHand highcard kicker _) (HighCardHand highcard' kicker' _) = compare (highcard, kicker) (highcard', kicker')


-- getHighCard :: Card -> Maybe HighCard -> HighCard
-- getHighCard card Nothing = HighCard card

-- findKicker :: HighCard -> Card -> Maybe Kicker -> Kicker
-- findKicker (HighCard h) card Nothing = Kicker card
-- findKicker (HighCard h) k (Just (Kicker k')) =
--         case (compare h k, compare h k', compare k k') of
--             (EQ, EQ, EQ) -> Kicker k
--             (GT, EQ, LT) -> Kicker k
--             (GT, GT, GT) -> Kicker k
--             _            -> Kicker k'



hasAceHighCard :: FinalHandState -> Bool
hasAceHighCard (FinalHandState _ _ _ (HighCard card)) = case card of
            (Card (_, Ace)) -> True
            _               -> False

isRoyalFlush :: FinalHandState -> Bool
isRoyalFlush hs@(FinalHandState IsFlush IsStraight _ _) = hasAceHighCard hs
isRoyalFlush _                                          = False

isStraightFlush :: FinalHandState -> Bool
isStraightFlush hs@(FinalHandState IsFlush IsStraight  _ _) = (not . hasAceHighCard) hs
isStraightFlush _ = False

isFlush :: FinalHandState -> Bool
isFlush (FinalHandState IsFlush IsNotStraight _ _) = True
isFlush _                                          = False

isStraight :: FinalHandState -> Bool
isStraight (FinalHandState IsNotFlush IsStraight _ _) = True
isStraight _                                          = False

isFourOfAKind :: FinalHandState -> Bool
isFourOfAKind (FinalHandState _ _ (FourOfAKindMatch _) _) = True
isFourOfAKind _                                           = False

isFullHouse :: FinalHandState -> Bool
isFullHouse (FinalHandState _ _ (FullHouseMatch _ _) _) = True
isFullHouse _                                           = False

isThreeOfAKind :: FinalHandState -> Bool
isThreeOfAKind (FinalHandState _ _ (ThreeCardMatch _) _) = True
isThreeOfAKind _                                         = False

isTwoPair :: FinalHandState -> Bool
isTwoPair (FinalHandState _ _ (TwoPairMatch _ _) _) = True
isTwoPair _                                         = False

isPair :: FinalHandState -> Bool
isPair (FinalHandState _ _ (PairMatch _) _)=True
isPair _                                    = False

