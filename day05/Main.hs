module Main where

import Data.Bifunctor
import Data.IntSet (IntSet, fromAscList, size, unions)
import Data.Ix
import Data.List (sortBy)
import Debug.Trace (traceShow)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

type Range = (IngredientId, IngredientId)

type Ranges = [Range]

type IngredientId = Int

type IngredientIdSet = IntSet

isFresh :: IngredientId -> Ranges -> Bool
isFresh id = any (`inRange` id)

overlap :: Range -> Range -> Bool
overlap r1@(x1, y1) r2@(x2, y2) =
    inRange r1 x2
        || inRange r1 y2
        || inRange r2 x1
        || inRange r2 y1

-- | returns Nothing if the ranges do not overlap
rangeUnion :: Range -> Range -> Maybe Range
rangeUnion r1@(x1, y1) r2@(x2, y2)
    | overlap r1 r2 = Just (min x1 x2, max y1 y2)
    | otherwise = Nothing

rangeUnions :: Ranges -> Ranges
rangeUnions =
    foldl (flip unionWithRanges) []
        . sortBy
            ( \(x1, _) (x2, _) ->
                compare x1 x2
            )
  where
    unionWithRanges r [] = [r]
    unionWithRanges r (r1 : rs) = case rangeUnion r r1 of
        Nothing -> r1 : unionWithRanges r rs
        Just r' -> r' : rs

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

parseInput :: [String] -> Maybe (Ranges, [IngredientId])
parseInput s = liftA2 (,) (parseRanges sranges) (parseIds sids)
  where
    (sranges, sids) = second (drop 1) $ break null s
    parseIds = mapM readMaybe
    parseRanges = mapM parseRange
    parseRange s =
        let
            (sLow, sHigh) = second (drop 1) $ break (== '-') s
         in
            liftA2 (,) (readMaybe sLow) (readMaybe sHigh)

main :: IO ()
main = do
    minput <- parseInput <$> readInput
    case minput of
        Nothing -> fail "Invalid Input"
        Just (ranges, ids) -> do
            let freshIds = filter (`isFresh` ranges) ids
                unions = rangeUnions ranges
                totalRange = sum (rangeSize <$> unions)
            printf "Part 1: %d\n" $ length freshIds
            printf "Part 2: %d\n" totalRange
