module Data.PokerHand (Hand(..), PokerCards(..), PossiblePokerCards(..), deck, unwrapDeck, deal, toHand, shuffleDeck, toPokerCards, sortPokerCards)
where

import           Data.Cards             (Card (..), Rank (..), Suit (..),
                                         getCardRank, ranks, suits)
import           Data.PokerHand.Builder (FinalHandState (..), FlushState (..),
                                         FourOfAKindRank (..), HandState (..),
                                         HighCardRank (..), Kicker (..),
                                         MatchingCards (..), PairRank (..),
                                         StraightState (..),
                                         ThreeOfAKindRank (..), buildHS,
                                         finalizeHandState)

import           Control.Monad.Random   (MonadRandom)
import           Data.List              (foldl', nub, sort)
import           System.Random.Shuffle  (shuffleM)


data Shuffled
data Unshuffled
newtype Deck a = Deck [Card] deriving Show

unwrapDeck :: Deck a -> [Card]
unwrapDeck (Deck cards) = cards

newtype PossiblePokerCards = PossiblePokerCards [Card] deriving Show

data FiveUnsortedCards
data FiveSortedCards
newtype PokerCards a = PokerCards [Card] deriving (Eq)

instance Show (PokerCards a) where
    show (PokerCards cards) = foldl' (\cs c -> cs ++ " " ++ c) "" (show <$> cards)


data Hand = HighCard HighCardRank Kicker (PokerCards FiveSortedCards)
          | Pair PairRank HighCardRank Kicker (PokerCards FiveSortedCards)
          | TwoPair PairRank PairRank HighCardRank (PokerCards FiveSortedCards)
          | ThreeOfAKind ThreeOfAKindRank HighCardRank Kicker (PokerCards FiveSortedCards)
          | Straight HighCardRank (PokerCards FiveSortedCards)
          | Flush HighCardRank (PokerCards FiveSortedCards)
          | FullHouse ThreeOfAKindRank PairRank (PokerCards FiveSortedCards)
          | FourOfAKind FourOfAKindRank HighCardRank (PokerCards FiveSortedCards)
          | StraightFlush HighCardRank (PokerCards FiveSortedCards)
          | RoyalFlush (PokerCards FiveSortedCards)


instance Show Hand where
    show (HighCard hr kr cards) =
            show cards ++ "\n" ++ show hr ++ ", " ++ show kr
    show (Pair pr hr kr cards) =
            show cards ++ "\n" ++  show pr ++ "\n"  ++ show hr ++ ", " ++ show kr
    show (TwoPair pr pr' hr cards) =
        show cards ++ "\n" ++ show pr ++ " and a " ++ show pr' ++ "\n" ++ show hr
    show (ThreeOfAKind tr hr kr cards) =
        show cards ++ "\n" ++ show tr ++ "\n" ++ show hr ++ ", " ++ show kr
    show (Straight hr cards) =
        show cards ++ "\n" ++ "Straight\n" ++ show hr
    show (Flush hr cards) =
        show cards ++ "\n" ++ "Flush\n" ++ show hr
    show (FullHouse  (ThreeOfAKindRank tr) (PairRank pr) cards) =
        show cards ++ "\n" ++ "Full House, " ++ show tr ++  "'s full of " ++ show pr ++ "'s"
    show (FourOfAKind fr hr cards) =
        show cards ++ "\n" ++ show fr ++ "\n" ++ show hr
    show (StraightFlush hr cards) =
        show cards ++ "\n" ++ "Straight Flush\n" ++ show hr
    show (RoyalFlush cards) =
        show cards ++ "\n" ++ "Royal Flush"

instance Eq Hand where
    (==) (HighCard highcardRank kicker _) (HighCard highcardRank' kicker' _) =
            (highcardRank, kicker) == (highcardRank', kicker')
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
            (quad, kicker) == (quad', kicker')
    (==) (StraightFlush highcardRank _) (StraightFlush highcardRank' _) =
           highcardRank == highcardRank'
    (==) (RoyalFlush _) (RoyalFlush _) = True
    (==) _ _ = False

instance Ord Hand where
    compare (RoyalFlush _ )(RoyalFlush  _) = EQ
    compare (RoyalFlush  _) _             = GT
    compare _ (RoyalFlush _) = LT
    compare (StraightFlush highcardRank _) (StraightFlush highcardRank' _) =
            compare highcardRank highcardRank'
    compare (StraightFlush _ _) _ = GT
    compare _ (StraightFlush _ _) = LT
    compare (FourOfAKind quad highcardRank _) (FourOfAKind quad' highcardRank' _) =
            compare (quad, highcardRank) (quad', highcardRank')
    compare FourOfAKind{} _ = GT
    compare _ FourOfAKind{} = LT
    compare (FullHouse threeRank twoRank _) (FullHouse threeRank' twoRank' _) =
            compare (threeRank, twoRank) (threeRank', twoRank')
    compare FullHouse{} _ = GT
    compare _ FullHouse{} = LT
    compare (Flush highcardRank _) (Flush highcardRank' _) =
             compare highcardRank highcardRank'
    compare (Flush _ _) _ = GT
    compare _ (Flush _ _) = LT
    compare (Straight highcardRank _) (Straight highcardRank' _) =
            compare highcardRank highcardRank'
    compare (Straight _ _) _ = GT
    compare _ (Straight _ _) = LT
    compare (ThreeOfAKind triad highcardRank kicker _ ) (ThreeOfAKind triad' highcardRank' kicker' _) =
            compare (triad, highcardRank, kicker) (triad', highcardRank', kicker')
    compare ThreeOfAKind{} _ = GT
    compare _ ThreeOfAKind{} = LT
    compare (TwoPair twoP1 twoP1' highcardRank _) (TwoPair twoP2 twoP2' highcardRank' _) =
            compare (twoP1, twoP1', highcardRank) (twoP2, twoP2', highcardRank')
    compare TwoPair{} _ = GT
    compare _ TwoPair{} = LT
    compare (Pair pair highcardRank kicker _) (Pair pair' highcardRank' kicker' _) =
            compare (pair, highcardRank, kicker) (pair', highcardRank', kicker')
    compare Pair{} _ = GT
    compare _ Pair{}  = LT
    compare (HighCard highcardRank kicker (PokerCards cards)) (HighCard highcardRank' kicker' (PokerCards cards')) =
            if comparison == EQ then compare (cards !! 2) (cards' !! 2) else comparison
                where comparison = compare (highcardRank, kicker) (highcardRank', kicker')


deck :: Deck Unshuffled
deck = Deck $ [ Card (suit, rank)
              | rank <- ranks
              , suit <- suits
              ]
shuffleDeck :: MonadRandom m => Deck Unshuffled -> m (Deck Shuffled)
shuffleDeck (Deck cards) = Deck <$> shuffleM cards

deal :: Deck Shuffled -> (PossiblePokerCards, PossiblePokerCards)
deal d = deal' d (PossiblePokerCards [], PossiblePokerCards [])

deal' :: Deck Shuffled -> (PossiblePokerCards, PossiblePokerCards) -> (PossiblePokerCards, PossiblePokerCards)
deal' (Deck d) ppc@(PossiblePokerCards p, PossiblePokerCards p')
    | length p == 5 && length p' == 5 = ppc
    | otherwise                       = deal' (Deck cs) (PossiblePokerCards (c: p), PossiblePokerCards (c': p'))
    where
        -- use of the head and tail functions in this context is safe
        -- due to Deck Shuffled only being exposed through smart constructors
        c  = head d
        c' = (head . tail) d
        cs = (tail . tail) d


toPokerCards :: PossiblePokerCards -> Maybe (PokerCards FiveUnsortedCards)
toPokerCards (PossiblePokerCards cards) = if (length . nub) cards /= 5
                        then Nothing
                        else Just (PokerCards cards)

sortPokerCards :: PokerCards FiveUnsortedCards -> PokerCards FiveSortedCards
sortPokerCards (PokerCards cards) = PokerCards $ sort cards

toHand :: PokerCards FiveSortedCards -> Hand
toHand cards =
    determineHand handState cards
        where handState = getHandState cards

determineHand :: FinalHandState -> PokerCards FiveSortedCards -> Hand
determineHand (FinalHandState IsFlush IsStraight NoMatches (HighCardRank Ace)) cards = RoyalFlush cards
determineHand (FinalHandState IsFlush IsStraight NoMatches highcardRank) cards = StraightFlush highcardRank cards
determineHand (FinalHandState IsFlush IsNotStraight NoMatches highcardRank) cards = Flush highcardRank cards
determineHand (FinalHandState IsNotFlush IsStraight NoMatches highcardRank) cards = Straight highcardRank cards
determineHand (FinalHandState _ _ (FullHouseMatch  three two) _) cards = FullHouse three two cards
determineHand (FinalHandState _ _ (FourCardMatch (FourOfAKindRank matchRank)) _) cards@(PokerCards cs) =
        FourOfAKind (FourOfAKindRank matchRank) (HighCardRank highcardRank) cards
                where highcardRank = maximumWhere (/= matchRank) (getCardRank <$> cs)
determineHand (FinalHandState _ _ (TwoPairMatch (PairRank matchRank) (PairRank matchRank')) _) cards@(PokerCards cs) =
        TwoPair (PairRank matchRank) (PairRank matchRank') (HighCardRank highcardRank) cards
          where highcardRank = maximumWhere (`notElem` [matchRank, matchRank']) (getCardRank <$> cs)
determineHand (FinalHandState _ _ (ThreeCardMatch three@(ThreeOfAKindRank matchRank)) _) cards@(PokerCards cs) =
         ThreeOfAKind three (HighCardRank highcardRank) (Kicker kicker) cards
           where
                cardRanks    = getCardRank <$> cs
                highcardRank = maximumWhere (/= matchRank) cardRanks
                kicker       = maximumWhere (`notElem` [highcardRank, matchRank]) cardRanks
determineHand (FinalHandState _ _ (PairMatch two@(PairRank matchRank)) _) cards@(PokerCards cs) =
        Pair two (HighCardRank highcardRank) (Kicker kicker) cards
        where
            cardRanks    = getCardRank <$> cs
            highcardRank = maximumWhere (/= matchRank) cardRanks
            kicker       = maximumWhere (`notElem` [matchRank, highcardRank]) cardRanks
determineHand (FinalHandState _ _ _ highcard) cards@(PokerCards cs) = HighCard highcard kicker cards
        where kicker = Kicker $ (getCardRank <$> cs) !! 3

getHandState :: PokerCards FiveSortedCards -> FinalHandState
getHandState (PokerCards cards) = finalizeHandState $ foldl' buildHS NewHandState cards

maximumWhere :: Ord a => (a -> Bool) -> [a] -> a
maximumWhere f xs = maximum $ filter f xs


