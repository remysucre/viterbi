{-# LANGUAGE Strict #-}

module Lib where

import Debug.Trace

data Nuc = A | C | T | G | N deriving (Show, Eq)
type S = Bool
data Col = Col {state :: S, score :: Float, chain :: [S]}

someFunc :: IO ()
someFunc = do
  fc <- readFile "NC_011297.fna"
  let _:ls = lines fc
  let dna = concat ls
  let os = map toNuc dna
  let v = foldl vsum v0 os
  let (Col {score=n1, chain=ss1}, Col {score=n0, chain=ss0}) =
        v (Col {state=False, score=0, chain=[False]}
          ,Col {state=True, score=0, chain=[True]})
  let ds1 = map (\b -> if b then '1' else '0') ss1
  putStrLn ds1
  print n1
  let ds0 = map (\b -> if b then '1' else '0') ss0
  putStrLn ds0
  print n0

toNuc :: Char -> Nuc
toNuc 'A' = A
toNuc 'C' = C
toNuc 'T' = T
toNuc 'G' = G
toNuc x = trace [x] N

-- type V = Int -> S -> (Float, [S])
type V = (Col, Col) -> (Col, Col)

v0 :: V
v0 (Col {chain=[False]}, Col {chain=[True]}) =
  (Col {state=False, score=log 0.996, chain=[False]}
  ,Col {state=True, score=log 0.004, chain=[True]})
v0 _ = undefined

vsum :: V -> Nuc -> V
vsum vp o = v . vp
  where v (Col {state=s0, score=s0f, chain=s0s}
          ,Col {state=s1, score=s1f, chain=s1s}) = (vnext s0, vnext s1)
          where vnext s = Col {state=s, score=log (e s o) + max av1 av0, chain=ss}
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
