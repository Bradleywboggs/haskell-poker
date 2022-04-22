module Data.Hand where

import qualified Data.List as L
import Data.Cards

newtype PlayerCards = PlayerCards [Card] deriving Show
newtype HighCard = HighCard Card deriving (Show, Eq)
instance Ord HighCard where
    compare (HighCard c) (HighCard c') = compare c c'

newtype Kicker = Kicker Card deriving (Show, Eq)
instance Ord Kicker where
    compare (Kicker c) (Kicker c') = compare c c'

newtype Pair = Pair (Card, Card) deriving (Show, Eq)
instance Ord Pair where
    compare (Pair p) (Pair p')  = compare p p'

newtype TwoPair = TwoPair (Pair, Pair) deriving (Show, Eq)
instance Ord TwoPair where
    compare (TwoPair cs) (TwoPair cs') = compare cs cs'

newtype ThreeOfAKind = ThreeOfAKind (Card, Card, Card) deriving (Show, Eq)
instance Ord ThreeOfAKind where
    compare (ThreeOfAKind cs) (ThreeOfAKind cs') = compare cs cs'

newtype FullHouse = FullHouse (ThreeOfAKind, Pair) deriving (Show, Eq)
instance Ord FullHouse where
    compare (FullHouse fh) (FullHouse fh') = compare fh fh'
newtype FourOfAKind = FourOfAKind (Card, Card, Card, Card) deriving (Show, Eq)
instance Ord FourOfAKind where
    compare (FourOfAKind cs) (FourOfAKind cs') = compare cs cs'

data Hand = HighCardHand HighCard Kicker PlayerCards
          | PairHand Pair Kicker PlayerCards 
          | TwoPairHand TwoPair Kicker PlayerCards 
          | ThreeOfAKindHand ThreeOfAKind Kicker PlayerCards 
          | Straight HighCard PlayerCards 
          | Flush HighCard PlayerCards 
          | FullHouseHand FullHouse PlayerCards
          | FourOfAKindHand FourOfAKind Kicker PlayerCards 
          | StraightFlush HighCard PlayerCards
          | RoyalFlush  PlayerCards 
        deriving (Show)

instance Eq Hand where
    (==) (HighCardHand highcard kicker _) (HighCardHand highcard' kicker' _) =
            (highcard, kicker)  == (highcard', kicker')
    (==) (PairHand pair kicker _) (PairHand pair' kicker' _)   = 
            (pair, kicker)      == (pair', kicker')
    (==) (TwoPairHand twoPair kicker _) (TwoPairHand twoPair' kicker' _) =
            (twoPair, kicker)   == (twoPair', kicker')
    (==) (ThreeOfAKindHand triad kicker _) (ThreeOfAKindHand triad' kicker' _) =
            (triad, kicker)     == (triad', kicker')
    (==) (Straight highcard _) (Straight highcard' _) =
            highcard == highcard'
    (==) (Flush highcard _) (Flush highcard' _) =
            highcard == highcard' 
    (==) (FullHouseHand fullhouse _) (FullHouseHand fullhouse' _) =
            fullhouse == fullhouse'
    (==) (FourOfAKindHand quad kicker _) (FourOfAKindHand quad' kicker' _)   = 
            (quad, kicker)      == (quad', kicker')
    (==) (StraightFlush highcard _) (StraightFlush highcard' _) =
           highcard == highcard'
    (==) (RoyalFlush _) (RoyalFlush _) = True
    (==) _ _                                                                 = False

instance Ord Hand where
    compare (RoyalFlush _) (RoyalFlush _) = EQ
    compare (RoyalFlush _) _             = GT
    compare _ (RoyalFlush _) = LT
    compare (StraightFlush highcard _) (StraightFlush highcard' _) = compare highcard highcard'
    compare (StraightFlush _ _) _ = GT
    compare _ (StraightFlush _ _) = LT
    compare (FourOfAKindHand quad kicker _) (FourOfAKindHand quad' kicker' _) = compare (quad, kicker) (quad', kicker')
    compare FourOfAKindHand{} _ = GT
    compare _ FourOfAKindHand{} = LT
    compare (FullHouseHand fh _) (FullHouseHand fh' _) = compare fh fh'
    compare FullHouseHand{} _ = GT
    compare _ FullHouseHand{} = LT
    compare (Flush highcard _) (Flush highcard' _) = compare highcard highcard'
    compare (Flush _ _) _ = GT
    compare _ (Flush _ _) = LT
    compare (Straight highcard _) (Straight highcard' _) = compare highcard highcard'
    compare (Straight _ _) _ = GT
    compare _ (Straight _ _) = LT
    compare (ThreeOfAKindHand triad kicker _ ) (ThreeOfAKindHand triad' kicker' _) = compare (triad, kicker) (triad', kicker')
    compare ThreeOfAKindHand{} _ = GT
    compare _ ThreeOfAKindHand{} = LT
    compare (TwoPairHand twoP kicker _) (TwoPairHand twoP' kicker' _) = compare (twoP, kicker) (twoP', kicker')
    compare TwoPairHand{} _ = GT
    compare _ TwoPairHand{} = LT
    compare (PairHand pair kicker _) (PairHand pair' kicker' _) = compare (pair, kicker) (pair', kicker')
    compare PairHand{} _ = GT
    compare _ PairHand{}  = LT
    compare (HighCardHand highcard kicker _) (HighCardHand highcard' kicker' _) = compare (highcard, kicker) (highcard', kicker')

hand :: [Card] ->  Maybe Hand
hand cards = case L.sortBy (flip compare)  cards of
     []                         -> Nothing
     cs | length cs /= 5        -> Nothing
     cs                         -> Just (determineHand $ getHandState (PlayerCards  cs))

type HandIndex = Int
data HandState = HandState { couldBeFlush :: Bool
                           , couldBeStraight :: Bool
                           , sumOfDifferences :: Int
                           , matches :: [(Rank, Int)]
                           , lastSeen :: Maybe (HandIndex, Card)
                           , currentHighCard :: Maybe Card
                           , currentKicker :: Maybe Card
                           }
                | NewHandState


instance Semigroup HandState  where
  (<>) NewHandState NewHandState   = NewHandState
  (<>) NewHandState hs@HandState{} = hs
  (<>) hs@HandState{} NewHandState = hs 
  (<>) HandState { couldBeFlush=f
                 , couldBeStraight=st
                 , sumOfDifferences=s
                 , matches=m
                 , lastSeen=ls@(Just (idx, Card(suit, rank)))
                 , currentHighCard=hc@(Just h)
                 , currentKicker=ck@(Just k)
                 }
       HandState { couldBeFlush=f'
                 , couldBeStraight=st'
                 , sumOfDifferences=s'
                 , matches=m' 
                 , lastSeen=ls'@(Just (idx', Card(suit', rank')))
                 , currentHighCard=hc'@(Just h')
                 , currentKicker=ck'@(Just k')
                 }  = HandState { couldBeFlush = f && f' && suit == suit'
                                , couldBeStraight = sumOfDiff == 4
                                , sumOfDifferences = sumOfDiff
                                , matches  = m <> m' <> [] 
                                , lastSeen = if idx > idx' then  ls else ls'
                                , currentHighCard = max <$> hc <*> hc'
                                , currentKicker = ck
                                }
                    where sumOfDiff = s + s' + abs (rankToInt rank - rankToInt rank') 

instance Monoid HandState where 
    mempty =  NewHandState                             

getHandState :: PlayerCards -> HandState
getHandState = undefined
--getHandState (PlayerCards cards)  = V.foldl' (<>) mempty  (V.map (cardToHandState . uncurry)  (V.zip (V.fromList [0..]) cards))

cardToHandState :: HandIndex -> Card -> HandState
cardToHandState idx card@(Card (suit, rank))  = HandState  { couldBeFlush = True
                                                           , couldBeStraight = False
                                                           , sumOfDifferences = 0
                                                           , matches = []
                                                           , lastSeen = Just (idx, card)
                                                           , currentHighCard = Just card
                                                           , currentKicker = Just card
                                                           }
                        
determineHand :: HandState -> Hand
determineHand cards 
    | isRoyalFlush cards = undefined
    | isStraightFlush cards = undefined
    | isFourOfAKind cards = undefined
    | isFullHouse cards = undefined
    | isThreeOfAKind cards = undefined
    | isTwoPair cards = undefined
    | isPair cards = undefined
    | otherwise    = undefined -- HighCardHand


isRoyalFlush :: HandState -> Bool
isRoyalFlush hs = undefined 

isStraightFlush :: HandState -> Bool
isStraightFlush = undefined

isFourOfAKind :: HandState -> Bool
isFourOfAKind = undefined

isFullHouse :: HandState -> Bool
isFullHouse = undefined

isFlush :: HandState -> Bool
isFlush = undefined

isStraight :: HandState -> Bool
isStraight  = undefined

isThreeOfAKind :: HandState -> Bool
isThreeOfAKind = undefined

isTwoPair :: HandState -> Bool
isTwoPair = undefined

isPair :: HandState -> Bool
isPair = undefined

