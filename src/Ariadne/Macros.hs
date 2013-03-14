{-# LANGUAGE TemplateHaskell #-}

module Ariadne.Macros where

import Language.Haskell.TH
import Data.Generics.Zipper
import Control.Monad

justE e = [| Just $e |]

cmatch :: Q Exp -> Q Exp
cmatch exp = do
  ConE conNm <- exp
  varNm      <- newName "e"
  lamCaseE [ match (asP varNm $ recP conNm []) (normalB $ justE $ varE varNm) []
           , match wildP (normalB [| Nothing |]) []
           ]

zmatch :: Q Exp -> Q Exp
zmatch exp = [| getHole >=> $(cmatch exp) |]
