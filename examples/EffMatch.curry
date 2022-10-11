---------------------------------------------------------------------
-- Examples to test efficient regular expressions matching.
-- Since they use internal data, it can only be used if the
-- library `RegExpEff` exports all entities.
---------------------------------------------------------------------

import RegExpEff


f :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> c -> Reg c s
f l a = sym l (\(c, _) -> if c == a then one else zero)

-- Example: regular expression abc
abc :: Reg Char Bool
abc = conc [0] (conc [0] (f [0] 'a') (f [0] 'b')) (f [0] 'c')

-- Example: regular expression ab
abLm :: Reg (Int, Char) Leftmost
abLm = conc [0] (symi [0] 'a') (symi [0] 'b')

-- Example: regular expression ac
acLl :: Reg (Int, Char) LeftLong
acLl = conc [0] (symi [0] 'a') (symi [0] 'c')

-- Example: regular expression bc
bcAll :: Reg (Int, Char) All
bcAll = conc [0] (symi [0] 'b') (symi [0] 'c')

--Example: regular expression cd
cdAllRange :: Reg (Int, Char) AllRange
cdAllRange = conc [0] (symi [0] 'c') (symi [0] 'd')

pmatchEffAbc :: Bool
pmatchEffAbc = matchEff abc "abc"

nmatchEffAbc :: Bool
nmatchEffAbc = matchEff abc "abcd"

submatchAbLm :: Leftmost
submatchAbLm = submatch abLm "aabbab"

submatchAcLl :: LeftLong
submatchAcLl = submatch acLl "bacde"

submatchBcAll :: All
submatchBcAll = submatch bcAll "bcbc"

grepBcAll :: [Int]
grepBcAll = grep bcAll "bcbcbcbcbc"

grepPosBcAll :: Int
grepPosBcAll = grepPos bcAll "bcbcbc"

submatchCdAR :: AllRange
submatchCdAR = submatch cdAllRange "cdccd"

grepShowEffCdAR :: [[Char]]
grepShowEffCdAR = grepShow cdAllRange "cd"

grepUnique :: [[Char]]
grepUnique = grepShowUnique cdAllRange "cdcdcd"

timesMatchEff :: [Char] ->  Bool
timesMatchEff = matchEff (times [0] 1 4 (conc [0] (f [0] 'a') (f [0] 'b')))

startMatchEff :: Bool
startMatchEff = match (conc [0] (start [0] (symc [0] (=='a')) True)
  (symc [0] (=='b'))) "ab"

endMatchEff :: Bool
endMatchEff = match (end [0] (symc [0] (=='c')) False) "abcabc"

bracketMatchEff :: [Char] -> Bool
bracketMatchEff = match (bracket [0] [Right ('a', 'c'), Left '!'])

negBracketMatchEff :: [Char] -> Bool
negBracketMatchEff = match (negBracket [0] [Right ('a', 'c'), Left '!'])

capAb :: [(Int, [[Char]])]
capAb = captureEff (conc [0] (symc [0,1] (=='a')) (symc [0,2] (=='b'))) "ab"

capEps :: [(Int, [[Char]])]
capEps = captureEff (conc [0] (conc [0] (eps [0,1])(symc [0,1] (=='a')))
  (symc [0,2] (=='b'))) "ab"

capRepAlt :: [Char] -> [(Int, [[Char]])]
capRepAlt = captureEff (rep [0] (alt [0,1] (symc [0,1,2] (=='a'))
  (symc [0,1,3] (=='c'))))

capTimes :: [Char] -> [(Int, [[Char]])]
capTimes = captureEff (times [0] 1 5 (symc [0,1] (=='a')))

capTimesSeq :: [(Int, [[Char]])]
capTimesSeq = captureEff (times [0] 1 3 (conc [0,1] (symc [0,1,2] (=='a'))
  (symc [0,1,3] (=='c')))) "acacac"

capRepSeq :: [(Int, [[Char]])]
capRepSeq = captureEff (rep [0] (conc [0,1] (symc [0,1,2] (=='a'))
  (symc [0,1,3] (=='c')))) "acac"
