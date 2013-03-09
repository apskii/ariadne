{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, LambdaCase, UnicodeSyntax #-}
module Ariadne where

import Ariadne.Types
import Ariadne.AST

import Prelude.Unicode
import Data.Generics.Zipper
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Control.Monad

mapHole ∷ (Typeable a, Typeable b) ⇒ (a → b) → Zipper h → Maybe (Zipper h)
mapHole f z = do
  x ← getHole z
  setHole' (f x) z

guarded ∷ (a → Maybe a) → a → Maybe a
guarded f z = f z `mplus` Just z

path ∷ Move h → Zipper h → [Zipper h]
path mv z = case mv z of
  Nothing → []
  Just z' → z' : path mv z'

lefts  = path left
rights = path right
downs  = path down
downs' = path down'
ups    = path up
uprs   = catMaybes ∘ map right ∘ ups
upls   = catMaybes ∘ map left  ∘ ups

tryQ ∷ Query h x → Move h
tryQ q z = const z <$> q z

zAny ∷ Query h ()
zAny _ = Just ()

-- Basic movings
moveDR z = msum [down' z, right z, listToMaybe (uprs z)]
moveDL z = msum [down' z, left  z, listToMaybe (upls z)]

-- Basic non-retrogradable movings
moveDR' z = down' z `mplus` right z
moveDL' z = down' z `mplus` left  z

-- Query-success walkings
nextDR q z = msum $ nextDR' q z : map (nextDR' q) (uprs z)
nextDL q z = msum $ nextDL' q z : map (nextDL' q) (upls z)

-- Query-success non-retrogradable walkings
nextDR' q z = msum $ tryQ q z : (down' z >>= nextDR' q) : map (nextDR' q) (rights z)
nextDL' q z = msum $ tryQ q z : (down  z >>= nextDL' q) : map (nextDL' q) (lefts  z)
nextUR' q z = msum $ tryQ q z : (up    z >>= nextUR' q) : map (tryQ    q) (rights z)
nextUL' q z = msum $ tryQ q z : (up    z >>= nextUL' q) : map (tryQ    q) (lefts  z)

-- Deeply N-th succeeded nextDR
nthDR ∷ Integer → Query h x → Move h
nthDR n q = nextDR q >=> f (n - 1)
  where
    f 0 = return
    f n = moveDR >=> nextDR q >=> f (n - 1)

-- Shallowly N-th succeeded nextDR
nthDR' ∷ Integer → Query h x → Move h
nthDR' n q = nextDR' q >=> f (n - 1)
  where
    f 0 = return
    f n = msum ∘ map (nextDR' q >=> f (n - 1)) ∘ uprs




