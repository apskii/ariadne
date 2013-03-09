## Examples (for details, look in Examples.hs)

```haskell
z1 = zp [| Just ((1, 2, 3), "4") |]
z2 = zp [| ["a", "b", "c", "d"]  |]
z3 = zp [| (1, (2, 3), 4, 5, 6)  |]
z4 = zp [| do putStrLn "rrrr"
              x <- getLine
              y <- do putStrLn "meaw"
                      z <- getLine
                      return z
              putStrLn (x ++ y)
        |]
```

Get second literal from arbitrary term:

```haskell
sndLit = nthDR 2 zLit >=> up >=> zExp
```

```haskell
testZp sndLit z1  -->   2
testZp sndLit z2  -->  "b"
testZp sndLit z3  -->   2
```

Replace every third integer literal with twice as large literal:

```haskell
twice (IntegerL x) = IntegerL (x * 2)

twiceThdIntL = nthDR 3 zIntegerL >=> mapHole twice >=> guarded (nthDR 2 zIntegerL >=> twiceThdIntL)
```

```haskell
testZp (twiceThdIntL >=> fz) z3  -->  (1, (2, 6), 4, 5, 12)
```

Insert debug-printing before each bind inside do's (just for IO for simplicity):

```haskell
insDbgPrint str = (:) $ NoBindS $ AppE (VarE $ mkName "putStrLn") (LitE $ StringL str)

insBindDbgPrint z = do
  zH          <- nextDR zBindS z
  BindS pat _ <- getHole zH
  up zH >>= mapHole (insDbgPrint ("Binding " ++ show pat ++ "..."))
        >>= guarded (nextDR zBindS >=> moveDR >=> insBindDbgPrint)
```

```haskell
testZp (insBindDbgPrint >=> fz) z4
-->
do System.IO.putStrLn "rrrr"
   putStrLn "Binding VarP x_0..."
   x_0 <- System.IO.getLine
   putStrLn "Binding VarP y_1..."
   y_1 <- do System.IO.putStrLn "meaw"
             putStrLn "Binding VarP z_2..."
             z_2 <- System.IO.getLine
             GHC.Base.return z_2
   System.IO.putStrLn (x_0 GHC.Base.++ y_1)
```