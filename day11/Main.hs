{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad
import Control.Monad.State
import Data.Functor
import Data.Map.Strict
import qualified Data.Map.Strict as M
import System.IO
import Text.Parsec (Parsec, parse)
import Text.Parsec.Char
import Text.Parsec.Combinator

data Output = Device Device | Output deriving (Eq, Show)
data Device = MkD String | DAC | FFT deriving (Eq, Show, Ord)
newtype Server = MkS (Map Device [Output]) deriving (Eq, Show)

stringToDevice :: String -> Device
stringToDevice = \case
    "dac" -> DAC
    "fft" -> FFT
    s -> MkD s

parseLabel :: Parsec String e String
parseLabel = mapM (const letter) [0 .. 2]

parseDeviceAndOutput :: Parsec String e (Device, [Output])
parseDeviceAndOutput = do
    device <- stringToDevice <$> parseLabel
    void (char ':' >> space)
    outputs <-
        fmap (\l -> if l == "out" then Output else Device $ stringToDevice l)
            <$> (parseLabel `sepBy1` space)
    return (device, outputs)

parseServer :: [String] -> Maybe Server
parseServer lines =
    MkS
        . M.fromList
        <$> mapM (eitherToMaybe . parse parseDeviceAndOutput "") lines
  where
    eitherToMaybe = either (const Nothing) Just

readInput :: IO [String]
readInput = do
    isClosed <- isEOF
    if not isClosed
        then liftA2 (:) getLine readInput
        else pure []

countPaths :: Server -> Int
countPaths (MkS pairs) = go (MkD "you")
  where
    go device = case M.lookup device pairs of
        Nothing -> 0
        Just outs ->
            sum $
                outs <&> \case
                    Output -> 1
                    Device d -> go d

type MemoState a = State (Map (Device, Bool, Bool) Int) a

countPaths' :: Server -> Int
countPaths' (MkS pairs) = evalState (go (MkD "svr") (False, False)) M.empty
  where
    go :: Device -> (Bool, Bool) -> MemoState Int
    go device (didDac, didFft) =
        case M.lookup device pairs of
            Nothing -> return 0
            Just outs ->
                sum
                    <$> ( outs `forM` \case
                            Output -> return $ fromEnum (didDac && didFft)
                            Device d ->
                                cachedOrTraverse
                                    d
                                    ( d == DAC || didDac
                                    , d == FFT || didFft
                                    )
                        )
    cachedOrTraverse :: Device -> (Bool, Bool) -> MemoState Int
    cachedOrTraverse d p@(b1, b2) = do
        map <- get
        case M.lookup (d, b1, b2) map of
            Nothing -> do
                res <- go d p
                modify (M.insert (d, b1, b2) res)
                return res
            Just x -> return x

main :: IO ()
main = do
    server <- do
        lines <- readInput
        maybe (fail "Invalid Input") return $ parseServer lines
    print $ countPaths server
    print $ countPaths' server
