{-# LANGUAGE OverloadedStrings #-}

-- | Pruning passes used to remove unused record subtrees.
module Dhall.Core.Prune
    ( pruneUselessRecordTrees
    ) where

import Data.Map.Strict (Map)
import Data.Text       (Text)
import Dhall.Syntax
    ( Binding (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , RecordField (..)
    , Var (..)
    , subExpressions
    )

import qualified Data.Map.Strict as Map
import qualified Dhall.Map
import qualified Lens.Micro      as Lens

data Usage = Usage
    { usageDirect   :: Bool
    , usageChildren :: Map Text Usage
    }

instance Semigroup Usage where
    Usage directA childrenA <> Usage directB childrenB =
        Usage
            { usageDirect = directA || directB
            , usageChildren = Map.unionWith (<>) childrenA childrenB
            }

instance Monoid Usage where
    mempty = Usage False mempty

isUsageEmpty :: Usage -> Bool
isUsageEmpty usage =
    not (usageDirect usage) && Map.null (usageChildren usage)

prependUsagePath :: [Text] -> Usage -> Usage
prependUsagePath [] usage = usage
prependUsagePath (field : fields) usage =
    Usage
        { usageDirect = False
        , usageChildren = Map.singleton field (prependUsagePath fields usage)
        }

usageFromPath :: [Text] -> Usage
usageFromPath [] = Usage True mempty
usageFromPath (field : fields) =
    Usage
        { usageDirect = False
        , usageChildren = Map.singleton field (usageFromPath fields)
        }

bumpIfSame :: Text -> Var -> Var
bumpIfSame binderName (V targetName index)
    | binderName == targetName = V targetName (index + 1)
    | otherwise                = V targetName index

fieldPathFrom :: Var -> Expr s a -> Maybe [Text]
fieldPathFrom target expression = do
    path <- fieldPathOrRootFrom target expression

    if null path then Nothing else Just path

fieldPathOrRootFrom :: Var -> Expr s a -> Maybe [Text]
fieldPathOrRootFrom target = go []
  where
    go acc (Note _ expression) = go acc expression
    go acc (Field expression (FieldSelection _ label _)) = go (label : acc) expression
    go acc (Var variable_)
        | variable_ == target = Just acc
    go _ _ = Nothing

collectUsage :: Var -> Expr s a -> Usage
collectUsage target expression
    | Just path <- fieldPathFrom target expression = usageFromPath path
collectUsage target (Var variable_)
    | variable_ == target = usageFromPath []
collectUsage target (Project expression_ (Left fields))
    | Just prefix <- fieldPathOrRootFrom target expression_ =
        foldMap (\field -> usageFromPath (prefix <> [field])) fields
    | otherwise = collectUsage target expression_
collectUsage target (Project expression_ (Right projectionExpression)) =
    collectUsage target expression_ <> collectUsage target projectionExpression
collectUsage target (Lam _ functionBinding body) =
    collectUsage target (functionBindingAnnotation functionBinding)
        <> collectUsage (bumpIfSame (functionBindingVariable functionBinding) target) body
collectUsage target (Pi _ binderName domain codomain) =
    collectUsage target domain <> collectUsage (bumpIfSame binderName target) codomain
collectUsage target (Let binding body) =
    let boundVariableUsage = collectUsage (V (variable binding) 0) body
        boundVariableIsUsed = not (isUsageEmpty boundVariableUsage)

        valueUsage
            | not boundVariableIsUsed = mempty
            | Just prefix <- fieldPathOrRootFrom target (value binding) =
                prependUsagePath prefix boundVariableUsage
            | otherwise = collectUsage target (value binding)

        annotationUsage
            | boundVariableIsUsed = maybe mempty (collectUsage target . snd) (annotation binding)
            | otherwise = mempty
    in
        annotationUsage
            <> valueUsage
            <> collectUsage (bumpIfSame (variable binding) target) body
collectUsage target expression =
    foldMap (collectUsage target) (Lens.foldMapOf subExpressions (:[]) expression)

pruneRecordLit :: Usage -> Expr s a -> Expr s a
pruneRecordLit usage expression =
    case expression of
        RecordLit fields
            | not (usageDirect usage) ->
                let keptFields =
                        [ ( fieldName
                          , recordField
                                { recordFieldValue =
                                    pruneRecordLit childUsage (recordFieldValue recordField)
                                }
                          )
                        | (fieldName, recordField) <- Dhall.Map.toList fields
                        , Just childUsage <- [Map.lookup fieldName (usageChildren usage)]
                        ]
                in  RecordLit (Dhall.Map.fromList keptFields)
        _ -> expression

-- | Remove record subtrees that are never selected from let-bound values.
pruneUselessRecordTrees :: Expr s a -> Expr s a
pruneUselessRecordTrees = go
  where
    go expression =
        case Lens.over subExpressions go expression of
            Let binding body ->
                let usage = collectUsage (V (variable binding) 0) body
                    prunedValue = pruneRecordLit usage (value binding)
                in  Let (binding { value = prunedValue }) body
            rewrittenExpression ->
                rewrittenExpression
