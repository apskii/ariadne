{-# LANGUAGE TemplateHaskell, UnicodeSyntax #-}

module Ariadne.AST where

import Ariadne.Types
import Ariadne.Macros
import Language.Haskell.TH
import Data.Generics.Zipper

zLit ∷ Query h Lit
zLit = getHole

zCharL       = $(zmatch [| CharL       |])
zStringL     = $(zmatch [| StringL     |])
zIntegerL    = $(zmatch [| IntegerL    |])
zRationalL   = $(zmatch [| RationalL   |])
zIntPrimL    = $(zmatch [| IntPrimL    |])
zWordPrimL   = $(zmatch [| WordPrimL   |])
zFloatPrimL  = $(zmatch [| FloatPrimL  |])
zDoublePrimL = $(zmatch [| DoublePrimL |])
zStringPrimL = $(zmatch [| StringPrimL |])


zExp ∷ Query h Exp
zExp = getHole

zVarE        = $(zmatch [| VarE        |])
zConE        = $(zmatch [| ConE        |])
zLitE        = $(zmatch [| LitE        |])
zAppE        = $(zmatch [| AppE        |])
zInfixE      = $(zmatch [| InfixE      |])
zUInfixE     = $(zmatch [| UInfixE     |])
zParensE     = $(zmatch [| ParensE     |])
zLamE        = $(zmatch [| LamE        |])
zLamCaseE    = $(zmatch [| LamCaseE    |])
zTupE        = $(zmatch [| TupE        |])
zUnboxedTupE = $(zmatch [| UnboxedTupE |])
zCondE       = $(zmatch [| CondE       |])
zMultiIfE    = $(zmatch [| MultiIfE    |])
zLetE        = $(zmatch [| LetE        |])
zCaseE       = $(zmatch [| CaseE       |])
zDoE         = $(zmatch [| DoE         |])
zCompE       = $(zmatch [| CompE       |])
zArithSeqE   = $(zmatch [| ArithSeqE   |])
zListE       = $(zmatch [| ListE       |])
zSigE        = $(zmatch [| SigE        |])
zRecConE     = $(zmatch [| RecConE     |])
zRecUpdE     = $(zmatch [| RecUpdE     |])


zMatch ∷ Query h Match
zMatch = getHole



zBody ∷ Query h Body
zBody = getHole



zGuard ∷ Query h Guard
zGuard = getHole



zStmt ∷ Query h Stmt
zStmt = getHole

zBindS   = $(zmatch [| BindS   |])
zLetS    = $(zmatch [| LetS    |])
zNoBindS = $(zmatch [| NoBindS |])
zParS    = $(zmatch [| ParS    |])



zRange ∷ Query h Range
zRange = getHole