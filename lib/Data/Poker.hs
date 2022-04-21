module Data.Poker  -- TODO: Limit exported types and functions i.e. (deck, hand, etc)
where 

import Data.List

data Suit    = Spades | Clubs | Hearts | Diamonds deriving (Show)

instance Eq Suit where
    (==) Spades Spades     = True
    (==) Clubs Clubs       = True
    (==) Hearts Hearts     = True
    (==) Diamonds Diamonds = True
    (==) _ _               = False

data Rank    = Two 
             | Three 
             | Four 
             | Five 
             | Six 
             | Seven 
             | Eight 
             | Nine 
             | Ten 
             | Jack 
             | Queen 
             | King 
             | Ace deriving (Show, Eq, Ord)

newtype Card = Card { uncard :: (Suit, Rank) } deriving (Show, Eq)
instance Ord Card where
    compare (Card (_, rank)) (Card (_, rank')) = compare rank rank'
    (<=)    (Card (_, rank)) (Card (_, rank')) = (<=)    rank rank' 

newtype Deck = Deck { undeck :: [Card] } deriving Show
newtype PlayerCards = PlayerCards (Card, Card, Card, Card, Card) deriving Show
newtype HighCard = HighCard Card deriving (Show, Eq)
newtype Kicker = Kicker Card deriving (Show, Eq)
newtype Pair = Pair (Card, Card) deriving (Show, Eq)
newtype TwoPair = TwoPair (Pair, Pair) deriving (Show, Eq)
newtype ThreeOfAKind = ThreeOfAKind (Card, Card, Card) deriving (Show, Eq)
newtype FullHouse = FullHouse (ThreeOfAKind, Pair) deriving (Show, Eq)
newtype FourOfAKind = FourOfAKind (Card, Card, Card, Card) deriving (Show, Eq)

data Hand = HighCardHand HighCard Kicker PlayerCards
          | PairHand Pair Kicker PlayerCards 
          | TwoPairHand TwoPair Kicker PlayerCards 
          | ThreeOfAKindHand ThreeOfAKind Kicker PlayerCards 
          | Straight HighCard PlayerCards 
          | Flush HighCard PlayerCards 
          | FullHouseHand FullHouse PlayerCards
          | FourOfAKindHand FourOfAKind Kicker PlayerCards 
          | StraightFlush PlayerCards
          | RoyalFlush  PlayerCards 
        deriving (Show)

instance Eq Hand where
    (==) (HighCardHand highcard kicker _) (HighCardHand highcard' kicker' _) =
            (highcard, kicker)  == (highcard', kicker')
    (==) (PairHand pair kicker _) (PairHand pair' kicker' _)   = 
            (pair, kicker)      == (pair', kicker')
    (==) (ThreeOfAKindHand triad kicker _) (ThreeOfAKindHand triad' kicker' _) =
            (triad, kicker)     == (triad, kicker)
    (==) (FourOfAKindHand quad kicker _) (FourOfAKindHand quad' kicker' _)   = 
            (quad, kicker)      == (quad', kicker')
    (==) _ _                                                                 = False
    -- TODO: Finish implementing Eq for Hand

-- TODO: define instance Ord Hand


hand :: [Card] ->  Maybe Hand
hand cards = case sortBy (flip compare) .  nub $ cards of
     []                         -> Nothing
     cs | length cs /= 5        -> Nothing
     cs@[c1, c2, c3, c4, c5]    -> Just (determineHand (PlayerCards (c1, c2, c3, c4, c5)))
     _                          -> Nothing

determineHand :: PlayerCards -> Hand
determineHand cards 
    | isRoyalFlush cards = undefined
    | isStraightFlush cards = undefined
    | isFourOfAKind cards = undefined
    | isFullHouse cards = undefined
    | isThreeOfAKind cards = undefined
    | isTwoPair cards = undefined
    | isPair cards = undefined
    | otherwise    = undefined -- HighCardHand
isRoyalFlush :: PlayerCards -> Bool
isRoyalFlush = undefined

isStraightFlush :: PlayerCards -> Bool
isStraightFlush = undefined

isFourOfAKind :: PlayerCards -> Bool
isFourOfAKind = undefined

isFullHouse :: PlayerCards -> Bool
isFullHouse = undefined

isFlush :: PlayerCards -> Bool
isFlush = undefined

isStraight :: PlayerCards -> Bool
isStraight  = undefined

isThreeOfAKind :: PlayerCards -> Bool
isThreeOfAKind = undefined

isTwoPair :: PlayerCards -> Bool
isTwoPair = undefined

isPair :: PlayerCards -> Bool
isPair = undefined

rankToInt :: Rank -> Int
rankToInt r = case r of 
        Two -> 2
        Three  -> 3
        Four ->  4
        Five ->  5
        Six ->  6
        Seven ->  7
        Eight ->  8
        Nine ->  9
        Ten -> 10
        Jack  -> 11
        Queen ->  12
        King ->  13
        Ace ->  14
     
-- smart constructor to be moved to own module, leaving the data constructors unexported
deck :: Deck
deck = Deck [ Card (Spades, Ace)
            , Card (Spades, King)
            , Card (Spades, Queen)
            , Card (Spades, Jack)
            , Card (Spades, Ten)
            , Card (Spades, Nine)
            , Card (Spades, Eight)
            , Card (Spades, Seven)
            , Card (Spades, Six)
            , Card (Spades, Five)
            , Card (Spades, Four)
            , Card (Spades, Three)
            , Card (Spades, Two)

            , Card (Clubs, Ace)
            , Card (Clubs, King)
            , Card (Clubs, Queen)
            , Card (Clubs, Jack)
            , Card (Clubs, Ten)
            , Card (Clubs, Nine)
            , Card (Clubs, Eight)
            , Card (Clubs, Seven)
            , Card (Clubs, Six)
            , Card (Clubs, Five)
            , Card (Clubs, Four)
            , Card (Clubs, Three)
            , Card (Clubs, Two)

            , Card (Hearts, Ace)
            , Card (Hearts, King)
            , Card (Hearts, Queen)
            , Card (Hearts, Jack)
            , Card (Hearts, Ten)
            , Card (Hearts, Nine)
            , Card (Hearts, Eight)
            , Card (Hearts, Seven)
            , Card (Hearts, Six)
            , Card (Hearts, Five)
            , Card (Hearts, Four)
            , Card (Hearts, Three)
            , Card (Hearts, Two)

            , Card (Diamonds, Ace)
            , Card (Diamonds, King)
            , Card (Diamonds, Queen)
            , Card (Diamonds, Jack)
            , Card (Diamonds, Ten)
            , Card (Diamonds, Nine)
            , Card (Diamonds, Eight)
            , Card (Diamonds, Seven)
            , Card (Diamonds, Six)
            , Card (Diamonds, Five)
            , Card (Diamonds, Four)
            , Card (Diamonds, Three)
            , Card (Diamonds, Two)
            ]

