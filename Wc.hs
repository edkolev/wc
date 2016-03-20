module Wc where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

main :: IO ()
main = do
  t <- TIO.getContents
  print $ wc t

wc :: T.Text -> WcValue
wc t = evalState (processInput (T.lines t)) initialState
  where initialState = WcState 0 0 0

processInput :: [T.Text] -> State WcState WcValue
processInput [] = get >>= (\s -> return $ WcValue (lcount s) (wcount s) (bcount s))
processInput (x:xs) = do
  modify (\s -> s { lcount = (lcount s) + 1,
                    wcount = wcount s + (length $ T.words x),
                    bcount = bcount s + (T.length x) + 1 })
  processInput xs
