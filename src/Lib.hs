{-# LANGUAGE Strict #-}

module Lib where

import Debug.Trace
-- import System.Environment

data Nuc = A | C | T | G | N deriving Show
type S = Bool

someFunc :: IO ()
someFunc = do
  fc <- readFile "NC_011297.fna"
  let _:ls = lines fc
  let dna = concat ls
  let os = map toNuc dna
  let v = foldl vnext v0 os
  let ((s1, ss1), (s2, ss2)) = v ((0, [False]), (0, [True]))
  let ds1 = map (\b -> if b then '1' else '0') ss1
  putStrLn ds1
  print s1
  let ds2 = map (\b -> if b then '1' else '0') ss2
  putStrLn ds2
  print s2

toNuc :: Char -> Nuc
toNuc 'A' = A
toNuc 'C' = C
toNuc 'T' = T
toNuc 'G' = G
toNuc x = trace [x] N

-- type V = Int -> S -> (Float, [S])
type V = ((Float, [S]), (Float, [S])) -> ((Float, [S]), (Float, [S]))

v0 :: V
v0 ((_, [False]), (_, [True])) = ((log 0.996, [False]), (log 0.004, [True]))

vnext :: V -> Nuc -> V
vnext vp o = v . vp
  where v ((s0f, s0s), (s1f, s1s)) = ((log (e True o) + max av10 av20, ss0), (log (e False o) + max av11 av21, ss1))
          where av10 = log (a True False) + s1f
                av20 = log (a False False) + s0f
                ss0 = if av10 > av20
                      then True : s1s
                      else False : s0s
                av11 = log (a True True) + s1f
                av21 = log (a False True) + s0f
                ss1 = if av11 > av21
                      then True : s1s
                      else False : s0s

-- maxf :: (S -> (Float, [S])) -> (Float, [S])
-- maxf f = if v1 > v2 then (v1, s1) else (v2, s2)
--   where (v1, s1) = f True
--         (v2, s2) = f False

e :: S -> Nuc -> Float
e True A = 0.291
e True C = 0.209
e True T = 0.291
e True G = 0.209
e False A = 0.169
e False C = 0.331
e False T = 0.169
e False G = 0.331
e _ _  = 0.0

a :: S -> S -> Float
a True True = 0.999
a True False = 0.001
a False False = 0.99
a False True = 0.01
