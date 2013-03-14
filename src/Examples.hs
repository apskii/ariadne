{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Language.Haskell.TH
import Data.Generics.Zipper

import Ariadne
import Ariadne.AST

zp = fmap toZipper . runQ

fz = return . fromZipper

testZp f ioz = do
  z <- ioz
  maybe (error ":'(") (putStrLn . pprint) (f z)

z1 = zp [| Just ((1, 2, 3), "4") |]
z2 = zp [| ["a", "b", "c", "d"]  |]
z3 = zp [| (1, (2, 3), 4, 5, 6)  |]
z4 = zp [| do putStrLn "rrrr"
              x <- getLine
              y <- do
                putStrLn "meaw"
                z <- getLine
                return z
              putStrLn (x ++ y)
        |]

-- Get second literal from arbitrary term
sndLit = nthDR 2 zLit >=> up >=> zExp

t_1_1 = testZp sndLit z1  -- >   2
t_1_2 = testZp sndLit z2  -- >  "b"
t_1_3 = testZp sndLit z3  -- >   2

-- Replace every third integer literal with twice as large literal
twice (IntegerL x) = IntegerL (x * 2)

twiceThdIntL = nthDR 3 zIntegerL >=> mapHole twice >=> guarded (nthDR 2 zIntegerL >=> twiceThdIntL)

t_2 = testZp (twiceThdIntL >=> fz) z3  -- >  (1, (2, 6), 4, 5, 12)

-- Insert debug-printing before each bind inside do's (just for IO for simplicity)
insDbgPrint str = (:) $ NoBindS $ AppE (VarE $ mkName "putStrLn") (LitE $ StringL str)

insBindDbgPrint z = do
  zH          <- nextDR zBindS z
  BindS pat _ <- getHole zH
  up zH >>= mapHole (insDbgPrint ("Binding " ++ show pat ++ "..."))
        >>= guarded (nextDR zBindS >=> moveDR >=> insBindDbgPrint)

t_3 = testZp (insBindDbgPrint >=> fz) z4
{-- >
do System.IO.putStrLn "rrrr"
   putStrLn "Binding VarP x_0..."
   x_0 <- System.IO.getLine
   putStrLn "Binding VarP y_1..."
   y_1 <- do System.IO.putStrLn "meaw"
             putStrLn "Binding VarP z_2..."
             z_2 <- System.IO.getLine
             GHC.Base.return z_2
   System.IO.putStrLn (x_0 GHC.Base.++ y_1)
-}