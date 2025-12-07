{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State (State, evalState, gets, modify')
import Data.Ix (Ix (inRange))
import Data.Maybe
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (traceShowId)
import System.IO
import Text.Printf

data Cell = Empty | Splitter | Source deriving (Eq)

instance Show Cell where
    show = \case
        Empty -> "."
        Splitter -> "^"
        Source -> "S"

type CellRow = Vector Cell

type Map = Vector CellRow

type Position = (Int, Int)

printMap :: Map -> IO ()
printMap map = forM_ map $ \row ->
    putStrLn (concatMap show $ V.toList row)

-- | Represents, at a given line, the offset of the beams
type BeamsOffsets = [Int]

parseCell :: Char -> Maybe Cell
parseCell = \case
    '.' -> Just Empty
    'S' -> Just Source
    '^' -> Just Splitter
    _ -> Nothing

parseMap :: [String] -> Maybe Map
parseMap lines = do
    listMap <- go lines
    unless (mapIsRectangle listMap) $
        fail "Non-rectangle map"
    return $ V.fromList $ fmap V.fromList listMap
  where
    mapIsRectangle m = case m of
        [] -> False
        (l : ls) -> let headLength = length l in all ((== headLength) . length) ls
    go [] = Just []
    go (l : ls) = liftA2 (:) (mapM parseCell l) (go ls)

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

sourcePosition :: Map -> Maybe Position
sourcePosition = V.ifoldl (\pos y row -> pos <|> ((,y) <$> V.elemIndex Source row)) Nothing

countSplits :: Map -> Position -> Int
countSplits m (x, y) = go (V.drop (y + 1) m) [x]
  where
    go :: Map -> BeamsOffsets -> Int
    go m offs = case V.uncons m of
        Nothing -> 0
        Just (head, rest) ->
            let
                filterInRange = filter (inRange (0, V.length head - 1))
                (newBeamsOffsets, splitCount) =
                    foldl
                        ( \(offsets, splitCount) off ->
                            if head V.! off == Splitter
                                then (filterInRange [off - 1, off + 1] ++ offsets, splitCount + 1)
                                else (off : offsets, splitCount)
                        )
                        ([], 0)
                        offs
                uniqueBeamsOffsets = S.toList $ S.fromList newBeamsOffsets
             in
                splitCount + go rest uniqueBeamsOffsets

type TimelineMemoState = [(Position, Int)]

positionIsValid :: Position -> Map -> Bool
positionIsValid (x, y) map = isJust $ map V.!? y >>= (V.!? x)

countTimeLines :: Map -> Position -> Int
countTimeLines m (x, y) = 1 + evalState (go' (x, y + 1)) []
  where
    go' :: Position -> State TimelineMemoState Int
    go' (x, y) = case m V.!? y >>= (V.!? x) of
        Nothing -> return 0
        Just Splitter -> do
            timelines <- mapM computeOrLookup $ filter (`positionIsValid` m) $ fmap (,y + 1) [x - 1, x + 1]
            return $ 1 + sum timelines
        Just _ -> computeOrLookup (x, y + 1)
    computeOrLookup :: Position -> State TimelineMemoState Int
    computeOrLookup pos = do
        mcache <- gets (lookup pos)
        case mcache of
            Nothing -> do
                res <- go' pos
                modify' ((pos, res) :)
                return res
            Just cache -> return cache

main :: IO ()
main = do
    mmap <- parseMap <$> readInput
    map <- case mmap of
        Nothing -> fail "Invalid Input"
        Just m -> return m
    printMap map
    source <- maybe (fail "No Source") return $ sourcePosition map
    printf "Part 1: %d\n" $ countSplits map source
    printf "Part 2: %d\n" $ countTimeLines map source
