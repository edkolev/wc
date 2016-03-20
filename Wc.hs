
import Data.Char

import qualified Data.Text    as T
import qualified Data.Text.IO as T

count char (chars, beforeWord, words, lines)
    | not (isSpace char) && beforeWord = (chars + 1, False, words + 1, lines)
    | '\n' == char                     = (chars + 1, True,  words, lines + 1)
    | isSpace char                     = (chars + 1, True,  words, lines)
    | otherwise                        = (chars + 1, False, words, lines)

wordCount = T.foldl (flip count) (0, True, 0, 0)

pretty (c, _, w, l) = putStrLn (unwords [show l, show w, show c])

main = do
    allInput <- T.getContents
    pretty (wordCount allInput)
