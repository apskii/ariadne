{-# LANGUAGE UnicodeSyntax #-}

module Ariadne.Types where

import Data.Generics.Zipper

type Query h x = Zipper h → Maybe x
type Move  h   = Zipper h → Maybe (Zipper h)