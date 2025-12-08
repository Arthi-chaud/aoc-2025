module Main where

import Control.Applicative (Alternative ((<|>)), liftA3)
import Data.Bifunctor
import Data.Char (isDigit, isSpace)
import Data.List (find, findIndex, singleton, sortBy)
import qualified Data.Set as S
import Debug.Trace
import System.IO
import Text.Printf
import Text.Read (readMaybe)

type JBox = (Int, Int, Int)

type Circuit = [JBox]

type Distance = Float

parseJBox :: String -> Maybe JBox
parseJBox s = case split ',' s of
    [a, b, c] -> liftA3 (,,) (readMaybe a) (readMaybe b) (readMaybe c)
    _ -> Nothing
  where
    split _ [] = []
    split del s = case break (== del) s of
        (before, after) -> before : split del (dropWhile (== del) after)

parseJBoxes :: [String] -> Maybe [JBox]
parseJBoxes = mapM parseJBox

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

distance :: JBox -> JBox -> Distance
distance (a, b, c) (a', b', c') =
    sqrt $
        fromIntegral $
            foldl
                (\acc (n, n') -> acc + square (n - n'))
                0
                [(a, a'), (b, b'), (c, c')]
  where
    square n = n * n

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

connectClosestJBoxes :: [JBox] -> [(JBox, JBox)]
connectClosestJBoxes boxes =
    fmap fst $
        sortBy (\a b -> snd a `compare` snd b) $
            (\[a, b] -> ((a, b), distance a b)) <$> subsequencesOfSize 2 boxes

orderCircuitsByLength :: [Circuit] -> [Circuit]
orderCircuitsByLength cs = snd <$> sortBy (\a b -> fst b `compare` fst a) ((\c -> (length c, c)) <$> cs)

connectJBoxes :: [(JBox, JBox)] -> (Maybe (JBox, JBox), [Circuit])
connectJBoxes pairs = go pairs (singleton <$> S.toList (foldl (\s (a, b) -> S.insert a $ S.insert b s) S.empty pairs))
  where
    go [] acc = (Nothing, acc)
    go ((b, b') : bs) acc =
        let (bc, acc1) = extractIf (b `elem`) acc
            (bc', acc2) = extractIf (b' `elem`) acc1
            (lastconnect, rest) = case (bc, bc', acc2) of
                (Nothing, Nothing, acc') -> error "Oops"
                (Just c1, Nothing, acc') -> (Nothing, c1 : acc')
                (Nothing, Just c2, acc') -> (Nothing, c2 : acc')
                (Just c1, Just c2, []) -> (Just (b, b'), [c1 ++ c2])
                (Just c1, Just c2, acc') -> (Nothing, (c1 ++ c2) : acc')
         in first (lastconnect <|>) $ go bs rest
    extractIf f [] = (Nothing, [])
    extractIf f (x : xs) = if f x then (Just x, xs) else second (x :) $ extractIf f xs

main :: IO ()
main = do
    mboxes <- parseJBoxes <$> readInput
    boxes <- case mboxes of
        Nothing -> fail "Invalid input"
        Just boxes -> return boxes
    let pairs = connectClosestJBoxes boxes
    -- Part 1
    let (_, circuits) = connectJBoxes $ take 1000 pairs
    let orderedCircuits = orderCircuitsByLength circuits
        res = product $ length <$> take 3 orderedCircuits
    print res
    -- Part 2
    let Just ((x, _, _), (x', _, _)) = fst $ connectJBoxes pairs
    print $ x * x'
