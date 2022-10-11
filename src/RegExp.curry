------------------------------------------------------------------------------
--- A library to defines a structure of regular expressions and
--- a simple match operation for regular expressions.
--- This library is used to translate integrated code in the form
--- of POSIX extended regular expressions into Curry programs.
--- It is based on the bachelor thesis "Foreign Code Integration in Curry" of
--- Jasper Sikorra (March 2014).
---
--- @author Corinna Wambsganz
--- @version October 2022
------------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}


module RegExp
  ( RegExp, ORegExp(..), eps, literal, alt, conc, rep, pl, anyL, bracket, negBracket
  , start, end, times, captureG
  , match, grep, grepPos, grepShow, grepShowUnique, capture
  ) 
 where

import Data.List
import Control.AllValues ( allValues )

------------------------------------------------------------------------------
--- Data type for regex representation in Curry.
type RegExp a = [ORegExp a]

data ORegExp a = Nil
               | Literal a
               | Xor (RegExp a) (RegExp a)
               | Star (RegExp a)
               | Plus (RegExp a)
               | AnyLiteral
               | Bracket [Either a (a,a)]
               | NegBracket [Either a (a,a)]
               | Start (RegExp a)
               | End (RegExp a)
               | Times (Int,Int) (RegExp a)
               | Capture Int (RegExp a)


------------------------------------------------------------------------------
-- Interface operations for regular expressions, used by the Curry preprocessor.

--- ε
eps :: [Int] -> RegExp a
eps _ = [Nil]

--- a
literal :: [Int] -> a -> RegExp a
literal _ a = [Literal a]

--- α|β
alt :: [Int] -> RegExp a -> RegExp a -> RegExp a
alt _ p q = [Xor p q]

--- αβ
conc :: [Int] -> RegExp a -> RegExp a -> RegExp a
conc _ p q = p ++ q

--- α*
rep :: [Int] -> RegExp a -> RegExp a
rep _ r = [Star r]

--- α⁺
pl :: [Int] -> RegExp a -> RegExp a
pl _ r = [Plus r]

--- _
anyL :: [Int] -> RegExp a
anyL _ = [AnyLiteral]

--- [as]
bracket :: [Int] -> [Either a (a,a)] -> RegExp a
bracket _ l = [Bracket l]

--- [^as]
negBracket :: [Int] -> [Either a (a,a)]  -> RegExp a
negBracket _ l = [NegBracket l]

--- ^α
start :: [Int] -> RegExp a -> Bool -> RegExp a
start _ r _ = [Start r]

--- α$
end :: [Int] -> RegExp a -> Bool -> RegExp a
end _ r _ = [End r]

--- α{n,m}
times :: [Int] -> Int -> Int -> RegExp a -> RegExp a
times _ n m r = [Times (n,m) r]

--- /(α)/
captureG :: Int -> RegExp a -> RegExp a
captureG n r = [Capture n r]

------------------------------------------------------------------------------
-- Various operations on regular expressions.

--- The operation `grepPos` returns the first starting position of a substring
--- which matches the regular expression.
--- @param r - The regular expression
--- @param s - The input list
--- @result n - The startindex of the first substring that matches,
--- -1 if no substring matches
grepPos ::  (Data a, Ord a) => RegExp a -> [a] -> Int
grepPos re s = grepPos' re s 0
 where
  -- grepPos' :: (Data a, Ord a) => RegExp a -> [a] -> Int -> Int
  grepPos' _  []       _ = -1
  grepPos' r (x : xs) n = if match (r ++ [Star ([AnyLiteral])]) (x : xs)
                            then n
                            else grepPos' r xs (n+1)

--- The operation `grep` returns a list with starting positions of substrings
--- that match the regular expression.
--- @param r - The regular expression
--- @param s - The input list
--- @result l - The list of startingpositions of matching substrings
grep ::  (Data a, Ord a) => RegExp a -> [a] -> [Int]
grep re s = grep' re s 0
 where
  -- grep' :: (Data a, Ord a) => RegExp a -> [a] -> Int -> [Int]
  grep' _  []       _ = []
  grep' r (x : xs) n = if match (r ++ [Star ([AnyLiteral])]) (x : xs)
                            then (n : grep' r xs (n+1))
                            else grep' r xs (n+1)

--- The operation `grepShow` returns a list of substrings that match the regular
--- expression.
--- @param r - The regular expression
--- @param s - The input list
--- @return l - The list of substrings from s that match r
grepShow :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [[a]]
grepShow re s = allValues (grepShow' re s)
 where grepShow' re' s' | s' =:= _ ++ l ++ _ && match re' l = l
                          where l free

--- As `grepShow` but without duplicated elements.
grepShowUnique :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [[a]]
grepShowUnique re s = nub  (grepShow re s)

--- The match function is used to match lists with regular expressions
--- @param r - The regular expression
--- @param s - The input list
--- @result True if matched else False
match :: (Data a, Ord a) => RegExp a -> [a] -> Bool
match re s = case re of
  []                  -> s == []
  (Nil:ors)           -> match ors s
  (Xor or1 or2:ors)   -> match (or1 ++ ors) s || match (or2 ++ ors) s
  (Literal c:ors)     -> case s of
    []      -> False
    (d:ds)  -> if (d == c) then match ors ds else False
  (Star r:ors)        -> matchstar r ors s
  (Plus r:ors)        -> matchplus r ors s
  (AnyLiteral:ors)    -> case s of
    []      -> False
    (_:ds)  -> match ors ds
  (Bracket b:ors)     -> case s of
    []      -> False
    (d:ds)  -> (matchbracket b d) && match ors ds
  (NegBracket b:ors)  -> case s of
    []      -> False
    (d:ds)  -> not (matchbracket b d) && match ors ds
  (Start r:ors)       ->
    not . null . filter id . allValues $ matchstart (Start r:ors) s
  [End r]             ->
    not . null . filter id . allValues $ matchend (End r) s
  [End r, Nil]        ->
    not . null . filter id . allValues $ matchend (End r) s
  (End _ : _)         -> False
  (Times (n,m) r:ors) -> matchtimes s n m r ors
  (Capture _ r:ors)   -> match (r ++ ors) s

-- Matching start.
matchstart :: (Data a, Ord a) => RegExp a -> [a] -> Bool
matchstart (Start r : []) s = a ++ _ =:= s && match r a
 where a free
matchstart (Start r : ors@(_:_)) s = a ++ b =:= s && match r a && match ors b
 where a,b free

-- Matching end.
matchend :: (Data a, Ord a) => ORegExp a -> [a] -> Bool
matchend (End r) s = _ ++ b =:= s && match r b
  where b free

-- Matching with a star
matchstar :: (Data a, Ord a) => RegExp a -> RegExp a -> [a] -> Bool
matchstar r rgx st = (||)
  (match rgx st)
  (tryeach (map (\x -> match r x) (inits st)) (tails st) r rgx)

tryeach :: (Data a, Ord a) => [Bool] -> [[a]] -> RegExp a -> RegExp a -> Bool
tryeach [] []         _  _   = False
tryeach (b:bs) (t:ts) r  rgx =
  (||)
    (if b
      then
        (match rgx t || matchstar r rgx t)
      else False)
    (tryeach bs ts r rgx)

-- Matching with a plus
matchplus :: (Data a, Ord a) => RegExp a -> RegExp a -> [a] -> Bool
matchplus r rgx st = tryeach (map (\x -> match r x) ini) tls r rgx
  where
    ini = tail (inits st)
    tls = tail (tails st)

-- Matching with a bracket
matchbracket :: (Data a, Ord a) => [Either a (a,a)] -> a -> Bool
matchbracket [] _                          = False
matchbracket (Left c:es)        d | c == d = True
                                  | c /= d = matchbracket es d
matchbracket (Right (c1,c2):es) d          =
  (||)
    (d >= c1 && d <= c2)
    (matchbracket es d)

-- Matching an amount of times between a range
matchtimes :: (Data a, Ord a) => [a] -> Int -> Int -> RegExp a -> RegExp a
           -> Bool
matchtimes s n m r rgx | m == 0 = match rgx s
                       | m >  0 =
  tryeachRestricted (m-n) (map (\x -> match mr x) (inits s)) (tails s) r rgx
  where
    mr = concat (replicate n r)

tryeachRestricted :: (Data a, Ord a) => Int -> [Bool] -> [[a]] -> RegExp a
                  -> RegExp a -> Bool
tryeachRestricted _      []     []     _   _   = False
tryeachRestricted m      (b:bs) (t:ts) r  rgx  =
  (||)
    (if b
      then
        (match rgx t || matchtimes t 1 m r rgx)
      else False)
    (tryeachRestricted m bs ts r rgx)

--- The operation `capture` is used to return capture groups.
--- The capture group with number 0 is always the complete string.
--- @param r - The regular expression
--- @param s - The input list
--- @result l - The list with the capture groups and
--- the respectively matching substrings
capture :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [(Int, [[a]])]
capture re s = case re of
  []                  -> []
  (Nil:ors)           -> capture ors s
  (Xor or1 or2:ors)   -> if match (or1 ++ ors) s
                            then capture (or1 ++ ors) s
                            else capture (or2 ++ ors) s
  (Literal c:ors)     -> case s of
    []      -> []
    (d:ds)  -> if (d == c) then capture ors ds else []
  (Star r:ors)        -> con $ allValues $ captureStar (Star r : ors) s
  (Plus r:ors)        -> capture (r ++ [Star r] ++ ors) s
  (AnyLiteral:ors)    -> case s of
    []      -> []
    (_:ds)  -> capture ors ds
  (Bracket b:ors)     -> case s of
    []      -> []
    (d:ds)  -> if matchbracket b d
                 then capture ors ds
                 else []
  (NegBracket b:ors)  -> case s of
    []      -> []
    (d:ds)  -> if not (matchbracket b d)
                then capture ors ds
                else []
  (Start r:ors)       -> con $ allValues $ captureStart (Start r : ors) s
  [End r]             -> con $ allValues $ captureEnd (End r) s
  [End r, Nil]        -> con $ allValues $ captureEnd (End r) s
  (End _ : _)         -> []
  (Times (n,m) r:ors) -> con $ allValues $ captureTimes (Times (n,m) r : ors) s
  (Capture n r:ors)   -> con $ allValues $ captureCapture (Capture n r : ors) s

-- Finds the capture groups within a star.
captureStar :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [(Int, [[a]])]
captureStar (Star r : ors) s =
  if s =:= s1 ++ s2 && match ([Star r]) s1 && match ors s2
    then concat (map (capture r) (grepShow r s1)) ++ capture ors s2
    else []
      where s1, s2 free

-- Finds the capture groups within a start.
captureStart :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [(Int, [[a]])]
captureStart re s = case re of
  (Start r : [])  -> if match (Start r : []) s
                       then capture r (head (grepShow r s))
                       else []
  (Start r : ors) ->
    if s =:= s1 ++ s2 && match (Start r : ors) s && match ors s2
      then capture r s1 ++ capture ors s2
      else []
        where s1, s2 free

-- Finds the capture groups within an end.
captureEnd :: (Data a, Ord a, Show a) => ORegExp a -> [a] -> [(Int, [[a]])]
captureEnd (End r) s =
  if s =:= _ ++ s2 && match r s2
    then capture r s2
    else []
 where s2 free

-- Finds the capture groups within a times.
captureTimes :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [(Int, [[a]])]
captureTimes (Times (n,m) r : ors) s =
  if s =:= s1 ++ s2 && match ([Times (n,m) r]) s1 && match ors s2
    then concat (map (capture r) (grepShow r s1)) ++ capture ors s2
    else []
 where s1, s2 free

-- Finds capture groups.
captureCapture :: (Data a, Ord a, Show a) => RegExp a -> [a] -> [(Int, [[a]])]
captureCapture (Capture n r : ors) s =
  if s =:= s1 ++ s2 && match r s1 && match ors s2
    then [(n, [s1])] ++ capture r s1 ++ capture ors s2
    else []
 where s1, s2 free


-- Auxiliary operation to unpack result lists and clean.
con :: (Data a, Ord a) => [[(Int, [[a]])]] -> [(Int, [[a]])]
con li = comprime (concat li)

-- Gets a list with pairs of int and some lists and returns a list
-- where the pairs with the same int-number are combined.
comprime :: (Data a, Ord a) => [(Int, [[a]])] -> [(Int, [[a]])]
comprime []             = []
comprime ((n, as) : rl) = case filter (\(m, _) -> n == m) rl of
  [] -> (n, as) : comprime rl
  l  -> (n, as ++ concatMap snd l):comprime (filter (\(m, _) -> not(n == m)) rl)

------------------------------------------------------------------------------
