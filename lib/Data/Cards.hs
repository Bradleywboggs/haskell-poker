module Data.Cards where

import qualified Data.List as L

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

