module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Ix
import Data.List
import Data.Maybe (mapMaybe)
import Data.Tuple
import System.IO
import Text.Printf
import Text.Read (readMaybe)

type Coord = (Int, Int)
type Area = Int
type Edges = [(Coord, Coord)]
type Rectangle = (Coord, Coord)

parseCoord :: String -> Maybe Coord
parseCoord s = liftA2 (,) (readMaybe n1) (readMaybe $ drop 1 rest)
  where
    (n1, rest) = break (== ',') s

parseCoords :: [String] -> Maybe [Coord]
parseCoords = mapM parseCoord

coordRange :: Coord -> Coord -> [Coord]
coordRange (x, y) (x', y') =
    (\l -> (l,) <$> range (min y y', max y y')) `concatMap` range (min x x', max x x')

area :: Coord -> Coord -> Area
area (x, y) (x', y') = (1 + abs (x - x')) * (1 + abs (y - y'))

areaInMap :: (Coord, Coord) -> [Coord] -> Bool
areaInMap (a, b) map = all (`elem` map) $ coordRange a b

-- Source: https://stackoverflow.com/questions/21265454/subsequences-of-length-n-from-list-performance
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
    let l = length xs
     in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
        let next = subsequencesBySize xs
         in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

areas :: [Coord] -> [((Coord, Coord), Area)]
areas = fmap (\[a, b] -> ((a, b), area a b)) . subsequencesOfSize 2

printMap :: [Coord] -> IO ()
printMap coords =
    let orderedCoords = sort coords
        cells = coordRange (0, 0) (10, 15)
     in forM_ cells $ \c@(x, y) -> do
            when (y == 0) $ putStrLn ""
            putStr $
                if swap c `elem` coords
                    then "X"
                    else "."

edges :: [Coord] -> Edges
edges [] = []
edges [a] = []
edges coords = pairs coords [(last coords, head coords)]
  where
    pairs :: [a] -> [(a, a)] -> [(a, a)]
    pairs [] acc = acc
    pairs [a] acc = acc
    pairs [a, b] acc = (a, b) : acc
    pairs (a : b : c) acc = (a, b) : pairs (b : c) acc

intersects :: Rectangle -> Edges -> Bool
intersects _ [] = False
intersects r@((minX, minY), (maxX, maxY)) (((ix, iy), (ix', iy')) : es) =
    let
        (iMinX, iMaxX) = minMax ix ix'
        (iMinY, iMaxY) = minMax iy iy'
        crosses = minX < iMaxX && maxX > iMinX && minY < iMaxY && maxY > iMinY
     in
        crosses || intersects r es

minMax :: Int -> Int -> (Int, Int)
minMax a b = if a < b then (a, b) else (b, a)

filterIntersecting :: [Coord] -> Edges -> [((Coord, Coord), Area)]
filterIntersecting [] _ = []
filterIntersecting [a] _ = []
filterIntersecting (a : b : c) edges =
    let
        rest = filterIntersecting (b : c) edges
     in
        ( ( \a' ->
                let
                    (minX, maxX) = minMax (fst a) (fst a')
                    (minY, maxY) = minMax (snd a) (snd a')
                 in
                    if not $ intersects ((minX, minY), (maxX, maxY)) edges
                        then Just ((a, a'), area a a')
                        else Nothing
          )
            `mapMaybe` (b : c)
        )
            ++ rest

main :: IO ()
main = do
    mcoords <- parseCoords <$> readInput
    coords <- case mcoords of
        Nothing -> fail "Invalid Input"
        Just c -> return c
    let allareas = areas coords
    -- Part 1
    let orderedAreas = sortBy (\a b -> snd b `compare` snd a) allareas
    printf "Part 1: %d\n" $ snd $ head orderedAreas
    -- Part

    let edges' = edges coords
    let filteredAreas = filterIntersecting coords edges'
    printf "Part 2: %d\n" $ snd $ maximumBy (\a b -> snd a `compare` snd b) filteredAreas
