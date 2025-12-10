{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Char (isDigit, isSpace)
import Data.Either
import Data.Functor
import Data.List (sortOn, subsequences)
import Data.Traversable (for)
import System.IO
import Text.Parsec

newtype LightDiagram = MkD [Bool] deriving (Show, Eq)

newtype Schematic = MkS [Int] deriving (Show, Eq)

newtype Joltage = MkJ [Int] deriving (Show, Eq)

data Machine = MkM
    { goalDiagram :: LightDiagram
    , schematics :: [Schematic]
    , jolt :: Joltage
    }
    deriving (Show)

parseLightDiagram :: Parsec String e LightDiagram
parseLightDiagram = between (char '[') (char ']') $ do
    lights <- fmap (== '#') <$> many (choice [char '.', char '#'])
    return $ MkD lights

parseNumberList :: Parsec String e [Int]
parseNumberList = (read <$> many1 (satisfy isDigit)) `sepBy` char ','

parseSchematic :: Parsec String e Schematic
parseSchematic =
    between (char '(') (char ')') $
        MkS <$> parseNumberList

parseJoltage :: Parsec String e Joltage
parseJoltage =
    between (char '{') (char '}') $
        MkJ <$> parseNumberList

parseMachine :: Parsec String e Machine
parseMachine = do
    goalDiagram <- parseLightDiagram
    skipMany1 space
    schematics <- many1 (parseSchematic <* space)
    jolt <- parseJoltage
    return $ MkM{..}

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

toggleAtIdx :: Int -> LightDiagram -> LightDiagram
toggleAtIdx _ (MkD []) = MkD []
toggleAtIdx 0 (MkD (a : as)) = MkD (not a : as)
toggleAtIdx n (MkD (a : as)) =
    let (MkD res) = toggleAtIdx (n - 1) (MkD as)
     in MkD (a : res)

pushButton :: Schematic -> LightDiagram -> LightDiagram
pushButton (MkS []) d = d
pushButton (MkS (n : ns)) lights = pushButton (MkS ns) $ toggleAtIdx n lights

shortestCombination :: [Schematic] -> LightDiagram -> [Schematic]
shortestCombination schs ld@(MkD l) = go combinations
  where
    go [] = []
    go (c : cs) =
        if foldl (flip pushButton) emptyD c == ld
            then c
            else go cs
    emptyD = MkD (replicate (length l) False)
    combinations = sortOn length $ subsequences schs

main :: IO ()
main = do
    machines <- do
        lines <- readInput
        either (fail . show) return $ mapM (parse parseMachine "") lines
    let combLengths =
            machines
                <&> ( \MkM{..} ->
                        length $ shortestCombination schematics goalDiagram
                    )
    print $ sum combLengths
