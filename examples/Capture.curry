---------------------------------------------------------------------
-- Examples for regular expressions capture operation operation
---------------------------------------------------------------------

import RegExp

capSeq :: [(Int, [[Char]])]
capSeq =
  capture [Capture 0 [Capture 1 [Literal 'a'], Capture 2 [Literal 'b']]] "ab"

capStar :: [(Int, [[Char]])]
capStar = capture [Capture 0 [Star [Capture 1 [Literal 'a']]]] "aa"

capTimes :: [(Int, [[Char]])]
capTimes = capture
  [Capture 0 [Times (1,3) [Capture 1 [Literal 'a', Literal 'c']]]] "acac"

capStart :: [(Int, [[Char]])]
capStart = capture [Capture 0 [Start [Capture 1 [Literal '0']]]] "0123"

capEnd :: [(Int, [[Char]])]
capEnd = capture [Capture 0 [End [Capture 1 [Literal 'z']]]] "abcz"

capTwice :: [(Int, [[Char]])]
capTwice = capture
  [Capture 0 [Star [Literal 'a']], Capture 1 [Star [Literal 'a']]] "a"
