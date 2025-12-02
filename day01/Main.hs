module Main where

import Control.Monad
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import System.IO
import Text.Read (readMaybe)

data Direction = L | R deriving (Eq, Read)

data Rotation = MkR {direction :: Direction, count :: Int}

parseRotation :: String -> Maybe Rotation
parseRotation s =
    let (dirChar, countStr) = splitAt 1 s
     in liftA2 MkR (readMaybe dirChar) (readMaybe countStr)

parseDocument :: [String] -> Maybe [Rotation]
parseDocument = mapM parseRotation

type RotationCombinationState = (Int, Int)

rotate :: Rotation -> State RotationCombinationState ()
rotate r = do
    (oldCurr, zeroCount) <- get
    let dir = if direction r == L then (-) else (+)
    let newRawCurr = oldCurr `dir` count r
    let newCurr = newRawCurr `mod` 100
        passedZeroCount = abs (newRawCurr `div` 100)
        -- Part 1
        -- newZeroCount = zeroCount + fromEnum (newCurr == 0)
        newZeroCount = zeroCount + passedZeroCount
    put (newCurr, newZeroCount)

-- 50 + R1000 => 50 - 1000 => -950
-- 50

readDocument :: IO [String]
readDocument = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readDocument
        else pure []

main :: IO ()
main = do
    mdoc <- parseDocument <$> readDocument
    case mdoc of
        Nothing -> fail "Invalid input"
        Just doc -> do
            let (_, zeroCount) = execState (forM_ doc rotate) (50, 0)
            print zeroCount
