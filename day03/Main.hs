module Main where

import Control.Monad
import Control.Monad.State (State, gets, modify', evalState)
import Data.Bifunctor
import Data.Char (digitToInt, isDigit)
import Data.List
import Debug.Trace
import System.IO

type Bank = [Int]

-- Part 1
-- largestJoltage :: Bank -> Int
-- largestJoltage [] = 0
-- largestJoltage b = largestDigit0 * 10 + largestDigit1
--   where
--     largestDigit1 = foldr max 0 $ drop (largestDigit0Idx + 1) b
--     (largestDigit0Idx, largestDigit0) =
--         foldr1
--             ( \prev@(_, highest)
--                curr@(_, value) -> if value > highest then curr else prev
--             )
--             $ zip [0 ..] (init b)

intListToInteger :: [Int] -> Integer
intListToInteger = foldl' (\acc curr -> acc * 10 + fromIntegral curr) 0

type MemoState = State [(([Int], Int), Integer)]

largestJoltage :: Bank -> MemoState Integer
largestJoltage b = go b  12
  where
    getOrCompute :: ([Int], Int) -> MemoState Integer
    getOrCompute idx = do
        memo <- gets $ lookup idx
        case memo of 
            Just res -> return res
            Nothing -> do 
                !res <- uncurry go idx 
                modify' ((idx, res):)
                return res
    go ::  [Int] -> Int -> MemoState Integer
    go [] _ = return 0
    go _ 0 = return 0
    go  l@(x : xs) limit = if length l == limit 
        then return $ intListToInteger l 
        else do
            a <- ((fromIntegral x * (10 ^ (limit - 1))) + )  <$> curry getOrCompute xs (limit - 1)
            b <-  curry getOrCompute xs limit
            return $ max a b

parseBank :: String -> Maybe Bank
parseBank s
    | (_, []) <- span isDigit s = Just $ fmap digitToInt s
    | otherwise = Nothing

parseBanks :: [String] -> Maybe [Bank]
parseBanks = mapM parseBank

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

main :: IO ()
main = do
    mbanks <- parseBanks <$> readInput
    case mbanks of
        Nothing -> fail "Invalid input"
        Just banks -> do
            let joltages =  (\b -> evalState  (largestJoltage b) []) <$>  banks
            print $ sum joltages
