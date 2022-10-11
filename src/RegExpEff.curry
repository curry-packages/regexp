------------------------------------------------------------------------------
--- An efficient library that defines a structure of regular expressions and
--- some operations for regular expressions.
--- This library is used to translate integrated code in the form
--- of POSIX extended regular expressions into efficient Curry programs.
--- The implementation is based on the paper "A Play on Regular Expressions -
--- Functional Pearl" by Fischer, Huch and Wilke (September 27–29, 2010,
--- Baltimore, Maryland, USA).
---
--- @author Corinna Wambsganz
--- @version October 2022
------------------------------------------------------------------------------

module RegExpEff
  ( Reg, eps, literal, alt, conc, rep, pl, anyL, bracket, negBracket
  , start, end, times, captureG
  , match, grep, grepPos, grepShow, grepShowUnique, capture
  ) 
 where

import Data.List

-- Datatype for regular expressions. The regular expression (reg) and two flags:
-- empty: true if the regular expression accepts the empty word and
-- final: true if the remaining regular expression accepts the empty word.
data Reg c s = Reg {emptyRe :: s, final :: s, cg :: [Int], reg :: Re c s}
data Re c s = Eps                       -- ε
            | Sym ((c, [Int]) -> s)     -- a
            | Alt (Reg c s) (Reg c s)   -- α|β
            | Seq (Reg c s) (Reg c s)   -- αβ
            | Rep (Reg c s)             -- α*
            | Start (Reg c s)           -- ^α
            | End (Reg c s)             -- α$
            | Times (Int,Int) (Reg c s) -- α{n,m}

-- Datatype to compute the leftmost position of a substring that
-- matches a given regular expression.
data Leftmost = NoLeft | Leftmost Startl
data Startl = NoStartl | Startl Int

-- Datatype to compute the range of a substring that matches a given regular
-- expression.
data LeftLong = NoLeftLong | LeftLong Range
data Range = NoRange | Range Int Int

-- Datatype to compute all startingpositions of substrings that match a given
-- regular expression.
data All = NoAll | All List
data List = NoList | List [Int]

-- Datatype to compute the range of all substrings that match a given regular
-- expression.
data AllRange = NoAllRange | AllRange Ranges
data Ranges = NoRanges | Ranges [(Int, Int)]

-- Datatype to compute capture groups.
data CaptureGroups = NoCaptureGroups | CaptureGroups Groups
data Groups = NoGroups | Groups ([(Int, [(Int, Int)])], [Int])

-- Eq-Instance for leftmost values.
instance Eq Leftmost where
  NoLeft     == NoLeft     = True
  Leftmost _ == Leftmost _ = True
  NoLeft     == Leftmost _ = False
  Leftmost _ == NoLeft     = False

-- Eq-Instance for leftlong values.
instance Eq LeftLong where
  NoLeftLong == NoLeftLong = True
  LeftLong _ == LeftLong _ = True
  NoLeftLong == LeftLong _ = False
  LeftLong _ == NoLeftLong = False

-- Eq-Instance for all values.
instance Eq All where
  NoAll == NoAll = True
  All _ == All _ = True
  NoAll == All _ = False
  All _ == NoAll = False

-- Eq-Instance for allRange values.
instance Eq AllRange where
  NoAllRange == NoAllRange = True
  AllRange _ == AllRange _ = True
  NoAllRange == AllRange _ = False
  AllRange _ == NoAllRange = False

-- Eq-Instance for captureGroup values.
instance Eq CaptureGroups where
  NoCaptureGroups == NoCaptureGroups = True
  CaptureGroups _ == CaptureGroups _ = True
  NoCaptureGroups == CaptureGroups _ = False
  CaptureGroups _ == NoCaptureGroups = False

-- A semiring is an algebraic structure with zero, one, addition, and
-- multiplication that satisfies certain laws.
class Semiring s where
 zero, one :: s
 (+) :: s -> s -> s
 (*) :: s -> s -> s

-- Semiring-Instance for boolean values.
instance Semiring Bool where
 zero = False
 one  = True
 (+)  = (||)
 (*)  = (&&)

-- Semiring-Instance for integer values.
instance Semiring Int where
 zero   = 0
 one    = 1
 (+) = (Prelude.+)
 (*) = (Prelude.*)

-- Semiring-Instance for leftmost values.
instance Semiring Leftmost where
 zero = NoLeft
 one = Leftmost NoStartl
 NoLeft     + NoLeft     = NoLeft
 Leftmost x + NoLeft     = Leftmost x
 NoLeft     + Leftmost x = Leftmost x
 Leftmost x + Leftmost y = Leftmost (leftmost x y)
  where leftmost NoStartl   NoStartl   = NoStartl
        leftmost NoStartl   (Startl i) = Startl i
        leftmost (Startl i) NoStartl   = Startl i
        leftmost (Startl i) (Startl j) = Startl (min i j)
 NoLeft     * NoLeft     = NoLeft
 NoLeft     * Leftmost _ = NoLeft
 Leftmost _ * NoLeft     = NoLeft
 Leftmost x * Leftmost y = Leftmost (startl x y)
  where startl NoStartl    s = s
        startl (Startl i)  _ = Startl i

-- Semiring-Instance for leftlong values.
instance Semiring LeftLong where
 zero = NoLeftLong
 one  = LeftLong NoRange
 NoLeftLong + NoLeftLong = NoLeftLong
 NoLeftLong + LeftLong x = LeftLong x
 LeftLong x + NoLeftLong = LeftLong x
 LeftLong x + LeftLong y = LeftLong (leftlong x y)
  where leftlong NoRange     NoRange     = NoRange
        leftlong NoRange     (Range i j) = Range i j
        leftlong (Range i j) NoRange     = Range i j
        leftlong (Range i j) (Range k l) | i < k || i == k && j >= l = Range i j
                                         | otherwise                 = Range k l
 NoLeftLong * NoLeftLong = NoLeftLong
 LeftLong _ * NoLeftLong = NoLeftLong
 NoLeftLong * LeftLong _ = NoLeftLong
 LeftLong x * LeftLong y = LeftLong (range x y)
  where range NoRange     NoRange     = NoRange
        range (Range i j) NoRange     = Range i j
        range NoRange     (Range i j) = Range i j
        range (Range i _) (Range _ j) = Range i j

-- Semiring-Instance for all values.
instance Semiring All where
 zero = NoAll
 one  = All NoList
 NoAll + NoAll = NoAll
 NoAll + All l = All l
 All l + NoAll = All l
 All x + All y = All (allList x y)
  where allList NoList   NoList   = NoList
        allList NoList   (List l) = List l
        allList (List l) NoList   = List l
        allList (List i) (List j) = List (j ++ i)
 NoAll * NoAll = NoAll
 NoAll * All _ = NoAll
 All _ * NoAll = NoAll
 All x * All y = All (list0 x y)
  where list0 NoList   NoList   = NoList
        list0 (List i) NoList   = List i
        list0 NoList   (List i) = List i
        list0 (List i) (List _) = List i

-- Semiring-Instance for allRange values.
instance Semiring AllRange where
 zero = NoAllRange
 one  = AllRange NoRanges
 NoAllRange + NoAllRange = NoAllRange
 NoAllRange + AllRange l = AllRange l
 AllRange l + NoAllRange = AllRange l
 AllRange x + AllRange y = AllRange (allrange x y)
  where allrange NoRanges   NoRanges   = NoRanges
        allrange NoRanges   (Ranges l) = Ranges l
        allrange (Ranges l) NoRanges   = Ranges l
        allrange (Ranges i) (Ranges j) = Ranges (j++i)
 NoAllRange * NoAllRange = NoAllRange
 NoAllRange * AllRange _ = NoAllRange
 AllRange _ * NoAllRange = NoAllRange
 AllRange x * AllRange y = AllRange (allr x y)
  where
    allr NoRanges   NoRanges = NoRanges
    allr (Ranges l) NoRanges   = Ranges l
    allr NoRanges   (Ranges l) = Ranges l
    allr (Ranges i) (Ranges j) = case i of
      []    -> Ranges j
      list1 -> case j of
        ((_,d) : js) -> Ranges (take ((length list1)-1) i ++
          [(fst (last list1),d)] ++ js)
        []           -> Ranges list1

-- Semiring-Instance for captureGroup values.
instance Semiring CaptureGroups where
   zero = NoCaptureGroups
   one  = CaptureGroups NoGroups
   NoCaptureGroups + NoCaptureGroups = NoCaptureGroups
   NoCaptureGroups + CaptureGroups l = CaptureGroups l
   CaptureGroups l + NoCaptureGroups = CaptureGroups l
   CaptureGroups x + CaptureGroups y = CaptureGroups (cgs x y)
    where
      cgs NoGroups   NoGroups   = NoGroups
      cgs (Groups l) NoGroups   = Groups l
      cgs NoGroups   (Groups l) = Groups l
      cgs (Groups (i, l1)) (Groups (j, l2)) = Groups ((conca i j),nub (l1++ l2))
      conca i j = case i of
        [] -> j
        ((n, s) : xs) -> case filter (\(nj, _) -> n == nj) j of
          [] -> (n, s) : conca xs j
          ((_, sj) : _) -> (n, s ++ sj) : conca xs
            (filter (\(nj, _) ->  not (n == nj)) j)
   NoCaptureGroups * NoCaptureGroups = NoCaptureGroups
   NoCaptureGroups * CaptureGroups _ = NoCaptureGroups
   CaptureGroups _ * NoCaptureGroups = NoCaptureGroups
   CaptureGroups x * CaptureGroups y = CaptureGroups (cgsm x y)
    where cgsm NoGroups   NoGroups   = NoGroups
          cgsm (Groups l) NoGroups   = Groups l
          cgsm NoGroups   (Groups l) = Groups l
          cgsm (Groups (i, l1)) (Groups (j, _)) = Groups ((comp i j l1), l1)
          comp i j l = case i of
            []           -> j
            ((n,s) : xs) -> case filter (\(nj,_) -> n == nj) j of
                []            -> (n,s) : comp xs j l
                ((_, sj) : _) -> (n, if elem n l then comb s sj else s ++ sj) :
                  comp xs (filter (\(nj, _) -> not (n == nj)) j) l
          comb _ []            = []
          comb s ((_, b) : xs) = map (\(a, _) -> (a, b)) s ++ comb s xs

-- Extends the semiring class with an index.
class Semiring s => Semiringi s where
 index :: Int -> s

-- Semiringi-Instance for leftmost values.
instance Semiringi Leftmost where
 index = Leftmost . Startl

-- Semiringi-Instance for leftlong values.
instance Semiringi LeftLong where
 index i = LeftLong (Range i i)

-- Semiringi-Instance for all values.
instance Semiringi All where
 index i = All (List [i])

--Semiringi-Instance for allRange values.
instance Semiringi AllRange where
 index i = AllRange (Ranges [(i,i)])

-- Extends the semiring class with a list.
class Semiring s => Semiringc s where
  list :: Int -> [Int] -> s

-- Semiringc-Instance for captureGroup values.
instance Semiringc CaptureGroups where
  list i l = CaptureGroups (Groups ((map (\n -> (n, [(i, i)])) l), l))

-- Semiringc-Instance for all values.
instance Semiringc All where
  list i _ = All (List [i])

-- Semiringc-Instance for allRange values.
instance Semiringc AllRange where
  list i _ = AllRange (Ranges [(i,i)])

-- Class with functions needed for the implementation of capture groups.
class CGFunction s where
  newcg :: s -> [Int] -> s
  justr :: s -> [Int] -> s

instance CGFunction Bool where
  newcg s _ = s
  justr = newcg

instance CGFunction Int where
  newcg s _ = s
  justr = newcg

instance CGFunction Leftmost where
  newcg s _ = s
  justr = newcg

instance CGFunction LeftLong where
  newcg s _ = s
  justr = newcg

instance CGFunction All where
  newcg s _ = s
  justr = newcg

instance CGFunction AllRange where
  newcg s _ = s
  justr = newcg

instance CGFunction CaptureGroups where
  newcg (NoCaptureGroups)               _ = NoCaptureGroups
  newcg (CaptureGroups (NoGroups))      _ = CaptureGroups (NoGroups)
  newcg (CaptureGroups (Groups (g, _))) l = CaptureGroups (Groups (g, l))
  justr (NoCaptureGroups)               _ = NoCaptureGroups
  justr (CaptureGroups (NoGroups))      _ = CaptureGroups (NoGroups)
  justr (CaptureGroups (Groups (g, _))) l = CaptureGroups (Groups
    (map (\(n, nl) -> if elem n l then (n, nl) else ((-abs(n)), nl)) g, l))

-- Functions for initializing a regex.
-- ε
eps :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s
eps l = Reg {emptyRe = one, final = zero, cg = l, reg = Eps}

--a
literal :: (Semiringc s, CGFunction s, Eq s) => [Int] -> Char
  -> Reg (Int, Char) s
literal l a = symc l (==a)

-- a
sym :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> ((c, [Int]) -> s)
  -> Reg c s
sym l f = Reg {emptyRe = zero, final = zero, cg = l, reg = Sym f}

-- a with index
symi :: (Semiringi s, CGFunction s, Eq s)=> [Int] -> Char -> Reg (Int, Char) s
symi l c = sym l weight
 where weight ((pos,x), _) | x == c    = index pos
                           | otherwise = zero

-- a with list
symc :: (Semiringc s, CGFunction s, Eq s)=> [Int] -> (Char -> Bool)
  -> Reg (Int, Char) s
symc l f = sym l captureList
 where captureList ((pos, x), li) | f x       = list pos li
                                  | otherwise = zero

-- α|β
alt :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s -> Reg c s
  -> Reg c s
alt l p q = Reg {emptyRe = newcg (emptyRe p + emptyRe q) l,
  final = newcg (final p + final q) l, cg = l, reg = Alt p q}

-- αβ
conc :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s -> Reg c s
  -> Reg c s
conc l p q = Reg {emptyRe = emptyRe p * emptyRe q,
  final = final p * emptyRe q + final q, cg = l, reg = Seq p q}

-- α*
rep :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s -> Reg c s
rep l r = Reg {emptyRe = one, final = newcg (final r) l, cg = l,
  reg = Rep r}

-- α⁺
pl :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s -> Reg c s
pl l r = conc l r (rep l r)

-- _
anyL :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Reg c s
anyL l = sym l (\_ -> one)

-- [as]
bracket :: (Semiringc s, CGFunction s, Eq s) => [Int] ->
  [Either Char (Char, Char)] -> Reg (Int, Char) s
bracket li l = let l' = map lift l
                   f  = foldl (\g h -> (\c -> (g c) || (h c))) (head l')
                     (tail l')
               in symc li (\c -> f c)

-- [^as]
negBracket :: (Semiringc s, CGFunction s, Eq s) => [Int] ->
  [Either Char (Char, Char)] -> Reg (Int, Char) s
negBracket li l = let l' = map negLift l
                      f  = foldl (\g h -> (\c -> (g c) && (h c)))(head l')
                        (tail l')
                  in symc li (\c -> f c)

-- ^α
start :: (Semiringc s, CGFunction s, Eq s) => [Int] -> Reg (Int, Char) s -> Bool
  -> Reg (Int, Char) s
start l r True  = Reg {emptyRe = emptyRe r, final = newcg (final r) l, cg = l,
   reg = Start r}
start l r False = conc l (Reg {emptyRe = emptyRe r, final = newcg (final r) l,
  cg = l, reg = Start r}) (rep l (symc l (\_ -> True)))

-- α$
end :: (Semiringc s, CGFunction s, Eq s) => [Int] -> Reg (Int, Char) s -> Bool
  -> Reg (Int, Char) s
end l r True  = Reg {emptyRe = emptyRe r, final = newcg (final r) l, cg = l,
  reg = End r}
end l r False = conc l (rep l (symc l (\_ -> True))) (Reg {emptyRe = emptyRe r,
  final = newcg (final r) l, cg = l, reg = End r})

-- α{n,m}
times :: (Semiring s, CGFunction s, Ord c, Eq s) => [Int] -> Int -> Int ->
  Reg c s -> Reg c s
times l n m r =
  if m < n || n < 0
    then Reg {emptyRe = zero, final = zero, cg = l, reg = Times (n,m) r}
    else if n == 0
           then Reg {emptyRe = one, final = final r, cg = l, reg =
             Times (n,m) r}
           else Reg {emptyRe = emptyRe r, final = final r * time n, cg = l,
             reg = Times (n,m) r}
             where time k = if k == 0 then one else zero

-- /(α)/
captureG :: (Semiring s, CGFunction s, Ord c, Eq s) => Int -> Reg c s -> Reg c s
captureG _ r = r


-- For Bracket.
lift :: Either Char (Char,Char) -> Char -> Bool
lift (Left a)      = \c -> c == a
lift (Right (a,b)) = \c -> (c >= a && c <= b)

-- For NegBracket.
negLift :: Either Char (Char,Char) -> Char -> Bool
negLift a c = not (lift a c)

-- The match function is used to match lists with regular expressions
-- efficient.
--- @param r - The regular expression
--- @param l - The input list
--- @result Semiring.one if matched else Semiring.zero
matchEff :: (Semiring s, CGFunction s, Ord c, Eq s) => Reg c s -> [c] -> s
matchEff r []     = emptyRe r
matchEff r (c:cs) = final (foldl (shift zero) (shift one r c) cs)

--- The shift function is used within the match function.
--- It marks (sets final) the right positions in the regular expression.
--- @param m - The mark
--- @param r - The regular expression
--- @param c - The next literal (of the input list)
--- @result reg - The marked regular expression
shift :: (Semiring s, CGFunction s, Ord c, Eq s) => s -> Reg c s -> c -> Reg c s
shift m r c = case reg r of
  Eps              -> eps (cg r)
  (Sym f)          -> (sym (cg r) f){final = (newcg (m * (f (c, cg r))) (cg r))}
  (Alt p q)        -> alt (cg r) (shift m p c) (shift m q c)
  (Seq p q)        -> conc (cg r) (shift m p c) (shift (m * emptyRe p + final p)
    q c)
  (Rep re)         -> rep (cg r) (shift (newcg (m + justr (final re) (cg r))
    (cg r)) re c)
  (Start re)       -> shift m re c
  (End re)         -> shift m re c
  (Times (i,j) re) -> let nre = (shift (newcg (m + justr (final re) (cg r))
                                 (cg r)) re c)
                      in times (cg r) (max (if (final nre == one) then i-1
                        else i) 0) (if (final nre == one) then j-1 else j) nre

--- The submatch function is used to check if a substring matches the regular
--- expression.
--- @param r - The regular expression
--- @param s - The input list
--- @result Semiring.one with indices if matched else Semiring.zero
submatch :: (Semiring s, CGFunction s, Ord c, Eq s) => Reg (Int, c) s -> [c]
  -> s
submatch r s = matchEff (conc [] arb (conc [] r arb)) (zip [0..] s)
 where arb = rep [] (sym [] (\_ -> one))

--- The grep function returns efficient a list with startingpositions of
--- substrings that match the regular expression.
--- @param r - The regular expression
--- @param s - The input list
--- @result l - The list of startingpositions of matching substrings
grep :: Ord a => Reg (Int, a) All -> [a] -> [Int]
grep re s = case submatch re s of
                 All (List l) -> l
                 _            -> []

--- The grepPos function returns efficient the first startingposition of a
--- substring which matches the regular expression.
--- @param r - The regular expression
--- @param s - The input list
--- @result n - The startindex of the first substring that matches,
--- -1 if no substring matches
grepPos :: Ord a => Reg (Int, a) All -> [a] -> Int
grepPos re s = case grep re s of
                    []  -> -1
                    pos -> head pos

--- The grepShow function returns efficient a list of substrings that match
--- the regular expression.
--- @param r - The regular expression
--- @param s - The input list
--- @return l - The list of substrings from s that match r
grepShow :: Ord a => Reg (Int, a) AllRange -> [a] -> [[a]]
grepShow re s = case submatch re s of
  AllRange (Ranges list0) -> map (\(i,j) -> drop i (take (j Prelude.+1)s)) list0
  _                       -> []

-- As grepShow but without doubles.
grepShowUnique :: Ord a => Reg (Int, a) AllRange -> [a] -> [[a]]
grepShowUnique re s = nub (grepShow re s)

--- The capture function is used to return capture groups efficient.
--- @param r - The regular expression
--- @param s - The input list
--- @result l - The list with the capture groups and
--- the respectively matching substrings
capture :: Ord c => Reg (Int, c) CaptureGroups -> [c] -> [(Int, [[c]])]
capture r s = case (matchEff r (zip [0.. ] s)) of
  CaptureGroups (Groups (l, _)) -> combine $ map (\(n, li) -> (n, (map (\(i, j)
    -> drop i (take (j Prelude.+ 1) s)) li))) l
  _                             -> []

-- Combines the strings of numbers with the same absolute value.
combine :: Ord c => [(Int, [[c]])] -> [(Int, [[c]])]
combine []              = []
combine ((n1, s1) : ns) = case filter (\(i, _) -> abs(i) == abs(n1)) ns of
    [] -> (abs(n1), s1) : combine ns
    li -> (abs(n1), foldr (++) [] $ map (\(_, s2) -> s2) ((n1, s1) : li)):
      combine (filter (\(i, _) -> not $ abs(i) == abs(n1)) ns)

-- Match with capture groups.
match :: Ord c => Reg (Int, c) CaptureGroups -> [c] -> Bool
match r s = let l = capture r s
            in if l == []
                 then False
                 else if (snd $ head l) == []
                        then False
                        else (head $ snd $ head l) == s
