{-# LANGUAGE BangPatterns, Strict #-}

module Lib where

import Debug.Trace
import System.Environment

data Nuc = A | C | T | G | N deriving Show
type S = Bool

someFunc :: IO ()
someFunc = do
  [arg] <- getArgs
  let n = read arg
  fc <- readFile "NC_011297.fna"
  let _:ls = lines fc
  let dna = concat ls
  let os = map toNuc dna
  let v = foldl vnext v0 os
  print (v n True)

toNuc :: Char -> Nuc
toNuc 'A' = A
toNuc 'C' = C
toNuc 'T' = T
toNuc 'G' = G
toNuc x = trace [x] N

type V = Int -> S -> (Float, [S])

v0 :: V
v0 _ False = (log 0.996, [False])
v0 _ True = (log 0.004, [True])

vnext :: V -> Nuc -> V
vnext vp o = v
  where v 0 False = (log 0.996, [False])
        v 0 True = (log 0.004, [True])
        v i j = (log (e j o) * (max av1 av2), ss)
          where av1 = log (a True j) + maxv
                av2 = log (a False j) + maxv
                (maxv, maxs) = maxf (vp (i-1))
                ss = if av1 > av2
                      then True : maxs
                      else False : maxs

maxf :: (S -> (Float, [S])) -> (Float, [S])
maxf f = if v1 > v2 then (v1, s1) else (v2, s2)
  where (v1, s1) = f True
        (v2, s2) = f False

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
