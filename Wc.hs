module Wc where

import Control.Monad.State
import Data.List

data WcState = WcState {
  lcount :: Int,
  wcount :: Int,
  bcount :: Int
}

data WcValue = WcValue {
  lineCount :: Int,
  wordCount :: Int,
  byteCount :: Int
} deriving (Show)

wc :: String -> WcValue
wc t = evalState (processInput (lines t)) initialState
  where initialState = WcState 0 0 0

processInput :: [String] -> State WcState WcValue
processInput [] = do
  s <- get
  return $ WcValue (lcount s) (wcount s) (bcount s)

processInput (x:xs) = do
  modify (\s -> s { lcount = (lcount s) + 1,
                    wcount = wcount s + (length $ words x),
                    bcount = bcount s + (length x) + 1 })
  processInput xs

main = do
  t <- getContents
  print $ wc t
