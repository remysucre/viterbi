{-# LANGUAGE Strict #-}

module Lib where

import Debug.Trace
import Data.List
import Numeric

data Nuc = A | C | T | G | N deriving (Show, Eq)
data Cell = Cell {state :: S, score :: Double, chain :: [S]}

type S = Bool
type V = (Cell, Cell) -> (Cell, Cell)

someFunc :: IO ()
someFunc = do
  fc <- readFile "NC_011297.fna"
  let l:ls = lines fc
      dna  = concat ls
      os   = map toNuc dna
  putStrLn "NC_011297.fna"
  putStrLn l
  (ss, _, _, _, _, _) <- iter os 5
  let gcs = islands ss
  putStrLn $ "GpC segments: " ++ show gcs
  print "DONE"

islands :: [S] -> [(Int, Int)]
islands ss =
  let segs  = group . reverse $ ss
      ls    = map length segs
      is    = scanl (+) 0 ls
      tls   = odds is
      _:hds = evens is
  in reverse $ zip hds tls

odds :: [Int] -> [Int]
odds ns = ms
  where (ms, _) = foldl (\(xs, ys) n -> (ys, n:xs)) ([], []) ns

evens :: [Int] -> [Int]
evens ns = ms
  where (_, ms) = foldl (\(xs, ys) n -> (ys, n:xs)) ([], []) ns

toNuc :: Char -> Nuc
toNuc 'A' = A
toNuc 'C' = C
toNuc 'T' = T
toNuc 'G' = G
toNuc x   = trace [x] N

v0 :: V
v0 (Cell {chain=[]}, Cell {chain=[]}) =
  (Cell {state=False, score=log 0.996, chain=[]}
  ,Cell {state=True, score=log 0.004, chain=[]})
v0 _ = undefined

type E = S -> Nuc -> Double

iter :: [Nuc] -> Int -> IO ([S], Int, Int, Int, Int, T)
iter _ 0 = return ([], 0, 0, 0, 0, a0)
iter os n = do
  (_, _, _, _, _, ap) <- iter os (n-1)
  let v = foldl (vsum ap) v0 os
      (Cell {score=n0, chain=ss0}, Cell {score=n1, chain=ss1}) =
        v (Cell {state=False, score=0, chain=[]}
          ,Cell {state=True, score=0, chain=[]})
      ss   = if n1 > n0 then ss1 else ss0
      ins  = count id ss
      outs = count not ss
      segs = group ss
      cgs  = length (filter or segs)
      ats  = length (filter (elem False) segs)
      t    = trans cgs ins ats outs
  putStrLn $ ">>>>>>>>>>>>>>> ITERATION " ++ show n
  printCounts ins outs cgs ats
  printTrans t
  return (ss, ins, outs, cgs, ats, t)

printTrans :: (S -> S -> Double) -> IO()
printTrans t = do
  putStrLn $ "a11 = " ++ showFFloat (Just 4) (t False False) ""
  putStrLn $ "a12 = " ++ showFFloat (Just 4) (t False True) ""
  putStrLn $ "a22 = " ++ showFFloat (Just 4) (t True True) ""
  putStrLn $ "a21 = " ++ showFFloat (Just 4) (t True False) ""

printCounts :: Int -> Int -> Int -> Int -> IO()
printCounts ins outs cgs ats = do
  putStrLn $ "s1 count: " ++ show outs
  putStrLn $ "s2 count: " ++ show ins
  putStrLn $ "s1 segments: " ++ show ats
  putStrLn $ "s2 segments: " ++ show cgs

trans :: Int -> Int -> Int -> Int -> S -> S -> Double
trans cgs ins ats outs =
  let t True True   = 1 - t True False
      t True False  = fromIntegral cgs / fromIntegral ins
      t False False = 1 - t False True
      t False True  = (fromIntegral ats-1) / fromIntegral (outs-1)
  in t

type T = S -> S -> Double

count :: (a -> Bool) -> [a] -> Int
count p = foldl (\c x -> c + if p x then 1 else 0) 0

vsum :: T -> V -> Nuc -> V
vsum a vp o = v . vp
  where v (Cell {state=s0, score=s0f, chain=s0s}
          ,Cell {state=s1, score=s1f, chain=s1s}) = (vnext s0, vnext s1)
          where vnext s = Cell { state=s
                               , score=log (e0 s o) + max av1 av0
                               , chain=ss}
                  where av0 = log (a s0 s) + s0f
                        av1 = log (a s1 s) + s1f
                        ss  = s : if av1 > av0 then s1s else s0s

e0 :: E
e0 s n = let Just f = lookup (s, n) et0 in f

type ET = [((S, Nuc), Double)]

et0 :: ET
et0 = [ ((False, A ), 0.291)
      , ((False, T ), 0.291)
      , ((False, C ), 0.209)
      , ((False, G ), 0.209)
      , ((True, A  ), 0.169)
      , ((True, T  ), 0.169)
      , ((True, C  ), 0.331)
      , ((True, G  ), 0.331)
      ]

a0 :: S -> S -> Double
a0 False False = 0.999
a0 False True  = 0.001
a0 True True   = 0.99
a0 True False  = 0.01
