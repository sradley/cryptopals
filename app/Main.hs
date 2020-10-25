module Main where

import Cryptopals.Challenges
import System.Environment    (getArgs)
import Text.Read
import Text.Printf

main :: IO ()
main = getArgs >>= main'
    where
        main' [n] = challenge' (readMaybe n :: Maybe Int)
        main'   _ = usage
        usage     = putStrLn "Usage: cabal run cryptopals <number>" 

challenge' :: Maybe Int -> IO ()
challenge'  Nothing = putStrLn "Please enter a valid number."
challenge' (Just n) =
    let
        output (Just fn) = fn
        output  Nothing = printf "Challenge '%d' not found.\n" n
    in
        output $ challenge n

