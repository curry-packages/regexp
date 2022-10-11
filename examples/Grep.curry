--------------------------------------------------------------------------
-- Examples for regular expressions grep-Functions with start/end anchors
--------------------------------------------------------------------------

import RegExp

-- Example: regular expression abc
abc :: RegExp Char
abc = [Literal 'a', Literal 'b', Literal 'c']

grepAbc :: [Int]
grepAbc = grep abc "abcdabcdeabc"

grepPosAbc :: Int
grepPosAbc = grepPos abc "dabcabc"

grepShowAbc :: [[Char]]
grepShowAbc = grepShow abc "abcdabcdabc"

grepShowUniqueAbc :: [[Char]]
grepShowUniqueAbc = grepShowUnique abc "abcdabcdabc"
