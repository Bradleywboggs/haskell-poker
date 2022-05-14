module Main where

import           Data.Bifunctor (bimap)
import           Data.Char      (toLower)
import           Data.Functor   ((<&>))
import           Data.Poker     (deal, deck, shuffleDeck, sortPokerCards,
                                 toHand, toPokerCards)


playPoker :: IO ()
playPoker = do
    (maybePlayerHand, maybeCpuPlayerHand) <- shuffleDeck deck
                                                <&> deal
                                                <&> bimap ((toHand . sortPokerCards <$>) . toPokerCards)
                                                          ((toHand . sortPokerCards <$>) . toPokerCards)

    case (maybePlayerHand, maybeCpuPlayerHand) of
        (Just playerHand, Just cpuPlayerHand) -> do
            putStrLn "\n\nYour cards: "  >> print playerHand
            putStrLn "\n\nTheir cards: " >> print cpuPlayerHand

            if playerHand > cpuPlayerHand then putStrLn "\n\nYou Win!\n\n" else putStrLn "\n\nYou Lose!\n\n"

            playAgain <- putStrLn "Do you want to play again? (Y/n)" >> getLine
            case toLower <$> playAgain of
                "n" -> putStrLn "Bye!"
                _   -> playPoker

        (_, _) -> putStrLn "Uh Oh... some dirty shenanigans must have happened. Bye!"


main :: IO()
main = do
    putStrLn "\n\n\nWelcome to Haskell Poker!\n\n\n"
    putStrLn "Press any key to begin" >> getLine >> playPoker

