{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This programm measures the time needed for matching/captureing a
--- regular expression with a string.
--- The syntax of regular expression is similar to
--- POSIX extended regular expressions.
------------------------------------------------------------------------------

import System.CPUTime
import RegExpEff -- if this is changed to RegExp, you have to wait for a long time...

-- This operation measures the time needed for matching a longer string.
timeMatch :: IO Int
timeMatch = do
  start <- getCPUTime
  if pmatch 1
    then do end <- getCPUTime
            return $ end - start
    else do end <- getCPUTime
            return $ end - start
                         
zs :: String
zs = replicate 9999 'z'

-- or ``regex a**********'' "a"
pmatch :: Int -> Bool
pmatch n | n == 0 = True
         | n > 0  = if match ``regex z*'' (zs ++ "1")
                      then pmatch (n-1)
                      else False

-- This operation measures the time which is needed to find the capture groups
-- from the file StuMail.txt.
timeCapture :: IO (Int, [(Int,[[Char]])])
timeCapture = do
  f     <- readFile "StuMail.txt"
  s     <- return (filter (/= '\n') $## f)
  start <- getCPUTime
  let l =  capture ``regex (stu/([0-9]*)/@mail.uni-kiel.de)*'' s
  end   <- getCPUTime
  return (end-start, l)
