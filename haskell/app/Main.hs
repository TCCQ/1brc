{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import System.IO (stdout, isEOF)
import GHC.Int
import Data.List (foldl', foldr, sortOn)
-- import Data.List.Split (splitOn)
import Data.HashMap.Strict hiding (singleton)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.IO
import Data.Hashable
import Control.Monad
import Prelude hiding (lines, splitAt, putStr, getLine, drop)


data WeatherStation =
  WeatherStation !Int64 !Int64 !Int64 !Int64
  deriving (Eq)
  -- min, max, sum, num readings

data WeatherReport =
  WeatherReport !T.Text !Int64
  deriving (Eq)

foldStep :: HashMap T.Text WeatherStation -> WeatherReport -> HashMap T.Text WeatherStation
foldStep !m (WeatherReport name val) =
  alter (updateStation val) name m

readLoop :: IO [WeatherReport]
readLoop =
  do
    done <- isEOF
    if done
      then
      return []
      else do
      h <- fmap parseLine getLine
      fmap (h:) readLoop

main :: IO ()
main = do
  iList <- readLoop
  let hmap = Data.List.foldl' foldStep
             (Data.HashMap.Strict.empty :: HashMap T.Text WeatherStation)
             iList
  sorted <- {-# SCC "main-sorting" #-} return $ Data.List.sortOn fst $! toList hmap
  putStr $ T.pack "{"
  printIter sorted
  putStr $ T.pack "}"

parseLine :: T.Text -> WeatherReport
parseLine !line =
  case T.split (== ';') line of
    [name,rest] -> case T.split (=='.') rest of
      [left,right] -> WeatherReport name $! (((read (T.unpack left))*10) + (read (T.unpack right)))
      _ -> error $ "Couldn't parse line" ++ T.unpack line
    _ -> error $ "Couldn't parse line" ++ T.unpack line

updateStation :: Int64 -> Maybe WeatherStation -> Maybe WeatherStation
updateStation !val !prior =
  case prior of
    Nothing -> Just $ WeatherStation val val val 1
    Just (WeatherStation minp maxp sump nump) ->
      Just $ WeatherStation (min minp val) (max maxp val) (sump + val) (nump+1)

fromFakeFloat :: Int64 -> String
fromFakeFloat !i =
 (show (i `div` 10)) ++ ('.':(show (i `mod` 10)))

printSingle :: (T.Text, WeatherStation) -> T.Text
printSingle (name, (WeatherStation min max sum num)) =
  T.concat $ name:(fmap T.pack
                 [
                   "=",
                   (fromFakeFloat min),
                   "/" ,
                   (fromFakeFloat max),
                   "/",
                   (show (((fromIntegral sum / 10.0 :: Double) / (fromIntegral num))))
                 ]
                )

printIter :: [(T.Text, WeatherStation)] -> IO ()
printIter [] = mempty
printIter (h:n:rest) =
  do
    Data.Text.IO.putStrLn $ printSingle h
    (printIter (n:rest))
printIter [h] = Data.Text.IO.putStrLn $ printSingle h  -- last element
