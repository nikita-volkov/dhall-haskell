{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This module implements the Dhall code minification pass, which combines:

    * Dead-code elimination (via 'Dhall.Core.DCE.dce')
    * Unused let-binding removal
    * Single-use let-binding inlining
    * Field-access beta-reduction on record literals
    * Common subexpression extraction (via 'Dhall.Core.CSE.cse')
-}
module Dhall.Core.Minify
    ( -- * Minification
      minify
    ) where

import Control.Applicative     ((<|>))
import Data.Text               (Text)
import Data.Void               (Void)
import Dhall.Syntax            ( Binding (..)
                               , Expr (..)
                               , FieldSelection (..)
                               , FunctionBinding (..)
                               , RecordField (..)
                               , Var (..)
                               , subExpressions
                               )
import Dhall.Syntax.Operations (shift)
import Dhall.Normalize         (freeIn, subst)
import Dhall.Core.DCE          (dce)
import Dhall.Core.CSE          (cse)

import qualified Dhall.Map
import qualified Lens.Micro as Lens

-- | Check whether an expression is, or recursively contains, an 'Assert'.
-- Assertion bindings are never removed or inlined because they enforce
-- type-checking constraints as a side effect.
isOrContainsAssert :: Expr s a -> Bool
isOrContainsAssert (Assert _) = True
isOrContainsAssert e          = Lens.anyOf subExpressions isOrContainsAssert e

-- | Count free occurrences of @V name 0@ in an expression.  Correctly handles
-- shadowing: entering a binder for @name@ increments the target de Bruijn
-- index so we continue tracking the right variable across nested scopes.
countFreeOccurrences :: Text -> Expr s a -> Int
countFreeOccurrences name = go 0
  where
    go idx = \case
        Var (V n i)
            | n == name && i == idx -> 1
            | otherwise             -> 0

        -- Binder: annotation is at current depth; body is one deeper if the
        -- introduced variable shadows @name@.
        Lam _ fb body ->
            go idx (functionBindingAnnotation fb)
            + go (if functionBindingVariable fb == name then idx + 1 else idx) body

        Pi _ x dom cod ->
            go idx dom
            + go (if x == name then idx + 1 else idx) cod

        Let b body ->
            go idx (value b)
            + maybe 0 (go idx . snd) (annotation b)
            + go (if variable b == name then idx + 1 else idx) body

        -- No new binders introduced; recurse into immediate subexpressions.
        e -> sum (map (go idx) (Lens.toListOf subExpressions e))

-- | Beta-reduce a field access on a record literal:
-- @{ k₁ = v₁, … }.k₁  ↦  v₁@
betaReduceField :: Expr s a -> Maybe (Expr s a)
betaReduceField (Field (RecordLit fields) (FieldSelection _ label _)) =
    fmap recordFieldValue (Dhall.Map.lookup label fields)
betaReduceField _ = Nothing

-- | Inline a let-binding whose variable appears exactly once in the body.
-- Never inlines bindings whose value is or contains an 'Assert'.
inlineSingleUse :: Expr s a -> Maybe (Expr s a)
inlineSingleUse (Let b body)
    | not (isOrContainsAssert (value b))
    , countFreeOccurrences (variable b) body == 1
    = let substituted = subst (V (variable b) 0) (value b) body
      in  Just (shift (-1) (V (variable b) 0) substituted)
inlineSingleUse _ = Nothing

-- | Remove a let-binding whose variable does not appear free in the body.
-- Never removes bindings whose value is or contains an 'Assert'.
removeUnused :: Eq a => Expr s a -> Maybe (Expr s a)
removeUnused (Let b body)
    | not (isOrContainsAssert (value b))
    , not (V (variable b) 0 `freeIn` body)
    = Just (shift (-1) (V (variable b) 0) body)
removeUnused _ = Nothing

{-| Minify a Dhall expression.

    Intended for use on fully-resolved, denoted expressions
    (@s = 'Void'@, no source annotations, no outstanding imports).

    The pass runs the following transformations to a fixed point:

    1. Dead-code elimination — removes record fields that are never accessed.
    2. Unused-binding removal — drops @let x = v in body@ when @x ∉ FV(body)@.
    3. Single-use inlining — substitutes @x@ when it appears exactly once.
    4. Field-access beta-reduction — @{ k = v }.k  ↦  v@.

    After the fixed-point, common subexpression extraction is applied once to
    lift repeated, closed subexpressions into shared @let@ bindings.
-}
minify :: Ord a => Expr Void a -> Expr Void a
minify = cse . fixpoint step
  where
    fixpoint f x =
        let x' = f x
        in  if x' == x then x else fixpoint f x'

    step = Lens.rewriteOf subExpressions rewrite . dce

    rewrite e = removeUnused e <|> inlineSingleUse e <|> betaReduceField e
