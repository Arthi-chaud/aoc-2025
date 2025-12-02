module Main where

import Data.Char
import Data.Ix
import Data.List (isPrefixOf)
import Debug.Trace
import Text.Read

type IdRange = (Int, Int)
type Id = Int

idIsInvalid :: Id -> Bool
idIsInvalid n = any (sequenceIsRepeated sN) sequences
  where
    sN = show n
    maxSeqLen = length sN `div` 2
    -- Part 1
    -- sequenceIsRepeated s seq = s == (seq ++ seq)
    sequenceIsRepeated s seq = case s of
        [] -> False
        _ ->
            (seq `isPrefixOf` s) && case drop (length seq) s of
                [] -> True
                s' -> sequenceIsRepeated s' seq
    sequences = go maxSeqLen
    go 0 = []
    go l = take l sN : go (l - 1)

invalidIdsInRange :: IdRange -> [Id]
invalidIdsInRange = filter idIsInvalid . range

parseRange :: String -> Maybe IdRange
parseRange s =
    let
        (n1, r1) = span isDigit s
        (n2, _) = span isDigit (drop 1 r1)
     in
        liftA2 (,) (readMaybe n1) (readMaybe n2)

parseRanges :: String -> Maybe [IdRange]
parseRanges s = case s of
    [] -> Just []
    _ ->
        let
            (strRange, rest) = span (/= ',') s
         in
            liftA2 (:) (parseRange strRange) (parseRanges (drop 1 rest))

main :: IO ()
main = do
    mranges <- parseRanges <$> getLine
    case mranges of
        Nothing -> fail "Invalid input"
        Just ranges ->
            let
                invalidIds = concatMap invalidIdsInRange ranges
             in
                print $ sum invalidIds
