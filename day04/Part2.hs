{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when, zipWithM)
import Data.Ix
import Data.Maybe (listToMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Debug.Trace (traceShowId)
import System.IO

data GridCell = MkC {hasPaperRoll :: !Bool, coord :: Coord}

data Grid = MkG {width :: Int, cells :: Vector GridCell}

type Coord = (Int, Int)

coordToIdx :: Coord -> Grid -> Int
coordToIdx (x, y) grid = y * width grid + x

pos :: Coord -> Grid -> Maybe GridCell
pos coord grid = cells grid !? coordToIdx coord grid

filterCells :: (GridCell -> Bool) -> Grid -> Vector GridCell
filterCells f = V.filter f . cells

removePaperRolls :: Vector Coord -> Grid -> Grid
removePaperRolls coords grid@MkG{..} =
    grid{cells = V.update cells (V.map (\coord -> (coordToIdx coord grid, MkC False coord)) coords)}

adjacentCells :: Coord -> Grid -> Vector GridCell
adjacentCells (x, y) grid = V.mapMaybe (`pos` grid) adjacentCoords
  where
    adjacentCoords =
        V.concatMap
            ( V.filter
                ( \coord@(x', y') ->
                    coord /= (x, y)
                        && inRange (0, width grid - 1) x'
                        && inRange (0, (V.length (cells grid) `div` width grid) - 1) y'
                )
                . \x' -> V.fromList [(x', y - 1), (x', y), (x', y + 1)]
            )
            (V.fromList [x, x - 1, x + 1])

adjacentPaperRollCount :: Coord -> Grid -> Int
adjacentPaperRollCount pos grid = length $ V.filter hasPaperRoll $ adjacentCells pos grid

removeAccessiblePaperRools :: Grid -> (Grid, Int)
removeAccessiblePaperRools grid =
    let
        accessiblePaperRolls = V.filter (\cell -> hasPaperRoll cell && adjacentPaperRollCount (coord cell) grid < 4) (cells grid)
        newGrid = removePaperRolls (V.map coord accessiblePaperRolls) grid
     in
        (newGrid, V.length accessiblePaperRolls)

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

parseGridCell :: Char -> Coord -> Maybe GridCell
parseGridCell c coord = case c of
    '.' -> Just $ MkC False coord
    '@' -> Just $ MkC True coord
    _ -> Nothing

parseGridRow :: String -> Int -> Maybe [GridCell]
parseGridRow srow y = zipWithM (\c x -> parseGridCell c (x, y)) srow [0 ..]

parseGrid :: [String] -> Maybe Grid
parseGrid srows = do
    rows <- zipWithM parseGridRow srows [0 ..]
    gridWidth <- length <$> listToMaybe rows
    if allSameRowLength rows gridWidth
        then Just $ MkG gridWidth (V.fromList $ concat rows)
        else Nothing
  where
    allSameRowLength grid l = all ((== l) . length) grid

printGrid :: Grid -> IO ()
printGrid grid = V.iforM_ (cells grid) $ \i cell -> do
    when (i `mod` width grid == 0) $ putStrLn ""
    putStr (if hasPaperRoll cell then "@" else ".")

main :: IO ()
main = do
    input <- readInput
    case parseGrid input of
        Nothing -> fail "Invalid input"
        Just grid -> do
            let loop g = case removeAccessiblePaperRools g of
                    (_, 0) -> 0
                    (newGrid, n) -> n + loop newGrid
            print $ loop grid
