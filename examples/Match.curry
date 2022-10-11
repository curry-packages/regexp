---------------------------------------------------------------------
-- Examples for regular expressions matching.
---------------------------------------------------------------------

import RegExp

-- Example: regular expression ^abc
abcStart :: RegExp Char
abcStart = [Start ([Literal 'a', Literal 'b', Literal 'c'])]

-- Example: regular expression abc$
abcEnd :: RegExp Char
abcEnd = [End ([Literal 'a', Literal 'b', Literal 'c'])]

ptestAbcStart :: Bool
ptestAbcStart = match abcStart "abcaaa"

ntestAbcStart :: Bool
ntestAbcStart = match abcStart "nabc"

ptestAbcEnd :: Bool
ptestAbcEnd = match abcEnd "aaaabc"

ntestAbcEnd :: Bool
ntestAbcEnd = match abcEnd "abcd"
