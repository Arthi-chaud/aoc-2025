{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (Arrow (first))
import Data.Char (isDigit, isSpace)
import Data.List (singleton, uncons)
import Debug.Trace
import System.IO
import Text.Read (readMaybe)

data MathOp = Add | Mult deriving (Eq, Show)

mathOpFunc = \case
    Add -> (+)
    Mult -> (*)

type MathSheet = ([[Int]], [MathOp])

type MathProblem = ([Int], MathOp)

resolveMathProblem :: MathProblem -> Int
resolveMathProblem (numbers, op) = foldl (mathOpFunc op) base numbers
  where
    base = if op == Add then 0 else 1

parseMathOp :: Char -> Maybe MathOp
parseMathOp = \case
    '*' -> Just Mult
    '+' -> Just Add
    _ -> Nothing

parseMathSheetLine :: String -> Maybe [Int]
parseMathSheetLine s = case dropWhile isSpace s of
    [] -> return []
    s' ->
        let
            (digits, rest) = span isDigit s'
         in
            liftA2 (:) (readMaybe digits) (parseMathSheetLine rest)

parseMathSheetOpLine :: String -> Maybe [MathOp]
parseMathSheetOpLine s = case dropWhile isSpace s of
    [] -> return []
    (c : s') -> liftA2 (:) (parseMathOp c) (parseMathSheetOpLine s')

parseMathSheet :: [String] -> Maybe MathSheet
parseMathSheet = \case
    [] -> Nothing
    [s] -> ([],) <$> parseMathSheetOpLine s
    (s : s') -> do
        numList <- parseMathSheetLine s
        (numLists, op) <- parseMathSheet s'
        return (numList : numLists, op)

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

toProblems :: MathSheet -> [MathProblem]
toProblems ([], _) = []
toProblems (_, []) = []
toProblems (lines, op : ops) = case splitHeads lines of
    Just (nums, rest) -> (nums, op) : toProblems (rest, ops)
    Nothing -> []
  where
    splitHeads :: [[a]] -> Maybe ([a], [[a]])
    splitHeads lists = do
        headsAndTails <- mapM uncons lists
        let heads = fst <$> headsAndTails
            tails = snd <$> headsAndTails
        return (heads, tails)

main :: IO ()
main = do
    msheet <- parseMathSheet <$> readInput
    case msheet of
        Nothing -> fail "Invalid Input"
        Just sheet -> do
            let problems = toProblems sheet
                results = resolveMathProblem <$> problems
            print $ sum results
