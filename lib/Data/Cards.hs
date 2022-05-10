module Data.Cards where

data Suit    = Spades | Clubs | Hearts | Diamonds deriving (Eq)
instance Show Suit where
    show Spades   = "♠️"
    show Clubs    = "♣️"
    show Hearts   = "♥️"
    show Diamonds = "♦️"


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
             | Ace deriving (Eq, Ord, Enum)

instance Show Rank where
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"



newtype Card = Card (Suit, Rank) deriving (Eq)
instance Show Card where
    show (Card (suit, rank)) = "[ "  ++ show rank ++ show suit ++ " ]"

instance Ord Card where
    compare (Card (_, rank)) (Card (_, rank')) = compare rank rank'

ranks :: [Rank]
ranks = [Two .. Ace]

suits :: [Suit]
suits = [Spades, Clubs, Hearts, Diamonds]

getCardRank :: Card -> Rank
getCardRank (Card (_, rank)) = rank

rankToInt :: Rank -> Int
rankToInt r = case r of
        Two   -> 2
        Three -> 3
        Four  -> 4
        Five  -> 5
        Six   -> 6
        Seven -> 7
        Eight -> 8
        Nine  -> 9
        Ten   -> 10
        Jack  -> 11
        Queen -> 12
        King  -> 13
        Ace   -> 14

isAdjacentRank :: Rank -> Rank -> Bool
isAdjacentRank r r'
    | r == Ace && r' == Two = True
    | r == Two && r' == Ace = True
    | otherwise             = abs (rankToInt r - rankToInt r') == 1
