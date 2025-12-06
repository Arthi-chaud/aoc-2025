{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (Arrow (first))
import Data.Char (isDigit, isSpace)
import Data.List (singleton, uncons, unsnoc)
import System.IO
import Text.Read (readMaybe)

data MathOp = Add | Mult deriving (Eq, Show)

mathOpFunc = \case
    Add -> (+)
    Mult -> (*)

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

parseNumberLines :: [String] -> Maybe [[Int]]
parseNumberLines s = do
    case splitHeads s of
        Nothing -> return []
        Just (heads, rests) -> case filter isDigit heads of
            [] -> ([] :) <$> parseNumberLines rests
            digits -> do
                number <- readMaybe digits
                numberGrid <- parseNumberLines rests
                case numberGrid of
                    [] -> Just [[number]]
                    (p : ps) -> Just ((number : p) : ps)

parseOpLine :: String -> Maybe [MathOp]
parseOpLine s = case dropWhile isSpace s of
    [] -> return []
    (c : s') -> liftA2 (:) (parseMathOp c) (parseOpLine s')

parseMathProblems :: [String] -> Maybe [MathProblem]
parseMathProblems s = do
    (numLines, opLine) <- unsnoc s
    nums <- parseNumberLines numLines
    ops <- parseOpLine opLine
    return $ zip nums ops

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

main :: IO ()
main = do
    mproblems <- parseMathProblems <$> readInput
    case mproblems of
        Nothing -> fail "Invalid Input"
        Just problems -> do
            let results = resolveMathProblem <$> problems
            print $ sum results

splitHeads :: [[a]] -> Maybe ([a], [[a]])
splitHeads lists = do
    headsAndTails <- mapM uncons lists
    let heads = fst <$> headsAndTails
        tails = snd <$> headsAndTails
    return (heads, tails)
