{-# LANGUAGE Strict #-}

module Lib where

import Debug.Trace

data Nuc = A | C | T | G | N deriving (Show, Eq)
data Cell = Cell {state :: S, score :: Float, chain :: [S]}

type S = Bool
type V = (Cell, Cell) -> (Cell, Cell)

someFunc :: IO ()
someFunc = do
  fc <- readFile "NC_011297.fna"
  let _:ls = lines fc
      dna = concat ls
      os = map toNuc dna
      v = foldl vsum v0 os
      (Cell {score=n1, chain=ss1}, Cell {score=n0, chain=ss0}) =
        v (Cell {state=False, score=0, chain=[False]}
          ,Cell {state=True, score=0, chain=[True]})
      ds1 = map (\b -> if b then '1' else '0') ss1
      ds0 = map (\b -> if b then '1' else '0') ss0
  putStrLn ds1
  print n1
  putStrLn ds0
  print n0

toNuc :: Char -> Nuc
toNuc 'A' = A
toNuc 'C' = C
toNuc 'T' = T
toNuc 'G' = G
toNuc x = trace [x] N

v0 :: V
v0 (Cell {chain=[False]}, Cell {chain=[True]}) =
  (Cell {state=False, score=log 0.996, chain=[False]}
  ,Cell {state=True, score=log 0.004, chain=[True]})
v0 _ = undefined

vsum :: V -> Nuc -> V
vsum vp o = v . vp
  where v (Cell {state=s0, score=s0f, chain=s0s}
          ,Cell {state=s1, score=s1f, chain=s1s}) = (vnext s0, vnext s1)
          where vnext s =
                  Cell {state=s, score=log (e s o) + max av1 av0, chain=ss}
                  where av0 = log (a s0 s) + s0f
                        av1 = log (a s1 s) + s1f
                        ss = if av1 > av0
                             then s : s1s
                             else s : s0s

e :: S -> Nuc -> Float
e False n
  | n==A || n==T = 0.291
  | n==C || n==G = 0.209
e True n
  | n==A || n==T = 0.169
  | n==C || n==G = 0.331
e _ _  = 0.0

a :: S -> S -> Float
a False False = 0.999
a False True = 0.001
a True True = 0.99
a True False = 0.01
