{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Data.Functor
import Data.List ((!?))
import Data.Maybe (catMaybes, mapMaybe)
import System.IO

newtype GridCell = MkC {hasPaperRoll :: Bool}

type GridRow = [GridCell]
type Grid = [GridRow]

pos :: (Int, Int) -> Grid -> Maybe GridCell
pos (x, y) grid = grid !? y >>= (!? x)

adjacentCells :: (Int, Int) -> Grid -> [GridCell]
adjacentCells (x, y) grid = mapMaybe (`pos` grid) adjacentPos
  where
    adjacentPos =
        concatMap
            (filter (/= (x, y)) . \x' -> [(x', y - 1), (x', y), (x', y + 1)])
            [x, x - 1, x + 1]

adjacentPaperRollCount :: (Int, Int) -> Grid -> Int
adjacentPaperRollCount pos grid = length $ filter hasPaperRoll $ adjacentCells pos grid

parseGridCell :: Char -> Maybe GridCell
parseGridCell = \case
    '.' -> Just $ MkC False
    '@' -> Just $ MkC True
    _ -> Nothing

parseGridRow :: String -> Maybe GridRow
parseGridRow = mapM parseGridCell

parseGrid :: [String] -> Maybe Grid
parseGrid srows = do
    rows <- mapM parseGridRow srows
    if allSameRowLength rows
        then Just rows
        else Nothing
  where
    allSameRowLength :: [GridRow] -> Bool
    allSameRowLength [] = True
    allSameRowLength (r : rs) = all ((== length r) . length) rs

mapPaperCells :: forall a. ((Int, Int) -> a) -> Grid -> [a]
mapPaperCells f grid = go grid 0
  where
    go :: Grid -> Int -> [a]
    go [] _ = []
    go (r : rs) y =
        fmap (\(x, _) -> f (x, y)) (filter (hasPaperRoll . snd) $ zip [0 ..] r)
            ++ go rs (y + 1)

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

main :: IO ()
main = do
    input <- readInput
    case parseGrid input of
        Nothing -> fail "Invalid input"
        Just grid -> do
            let adjacentCellsCounts = mapPaperCells (`adjacentPaperRollCount` grid) grid
                answer = length $ filter (< 4) adjacentCellsCounts
            print answer
