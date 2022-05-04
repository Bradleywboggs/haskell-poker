module Data.Cards where

-- import qualified Data.List as L

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
             | Ace deriving (Show, Eq, Ord, Enum)

newtype Card = Card { uncard :: (Suit, Rank) } deriving (Show, Eq)
instance Ord Card where
    compare (Card (_, rank)) (Card (_, rank')) = compare rank rank'
    (<=)    (Card (_, rank)) (Card (_, rank')) = (<=)    rank rank' 


rankToInt :: Rank -> Int
rankToInt r = case r of 
        Two    -> 2
        Three  -> 3
        Four   -> 4
        Five   -> 5
        Six    -> 6
        Seven  -> 7
        Eight  -> 8
        Nine   -> 9
        Ten    -> 10
        Jack   -> 11
        Queen  -> 12
        King   -> 13
        Ace    -> 14
    
isAdjacentRank :: Rank -> Rank -> Bool
isAdjacentRank r r' = abs (rankToInt r - rankToInt r') == 1