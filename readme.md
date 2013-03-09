## Examples (for details, look in Examples.hs)

- Get second literal from arbitrary term

        sndLit = nthDR 2 zLit >=> up >=> zExp

- Replace every third integer literal with twice as large literal

        twice (IntegerL x) = IntegerL (x * 2)

        twiceThdIntL = nthDR 3 zIntegerL >=> mapHole twice >=> guarded (nthDR 2 zIntegerL >=> twiceThdIntL)

- Insert debug-printing before each bind inside do's (just for IO for simplicity)

        insDbgPrint str = (:) $ NoBindS $ AppE (VarE $ mkName "putStrLn") (LitE $ StringL str)

        insBindDbgPrint z = do
          zH          ← nextDR zBindS z
          BindS pat _ ← getHole zH
          up zH >>= mapHole (insDbgPrint ("Binding " ++ show pat ++ "..."))
                >>= guarded (nextDR zBindS >=> moveDR >=> insBindDbgPrint)