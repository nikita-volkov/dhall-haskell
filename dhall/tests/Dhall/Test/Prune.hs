{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Prune where

import Data.Text    (Text)
import Data.Void    (Void)
import Test.Tasty   (TestTree)

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified Dhall.Core                as Core
import qualified Dhall.Parser              as Parser
import qualified Dhall.Pretty              as Pretty
import qualified Dhall.Test.Util           as Test.Util
import qualified Prettyprinter.Render.Text as Doc.Render.Text
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty.HUnit
import qualified Turtle

pruneDirectory :: FilePath
pruneDirectory = "./tests/Dhall/Test/Prune"

getTests :: IO TestTree
getTests = do
    pruneTests <- Test.Util.discover (Turtle.chars <* ".in.dhall") pruneTest (Turtle.lstree pruneDirectory)

    let testTree = Tasty.testGroup "Prune" [ pruneTests ]

    return testTree

format :: Core.Expr Void Core.Import -> Text
format expr =
    let renotedExpr = Core.renote expr :: Core.Expr Parser.Src Core.Import
        doc         = Pretty.prettyCharacterSet Pretty.Unicode renotedExpr <> "\n"
        docStream   = Pretty.layout doc
    in
        Doc.Render.Text.renderStrict docStream

pruneTest :: Text -> TestTree
pruneTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> ".in.dhall")
        let outputFile = Text.unpack (prefix <> ".out.dhall")

        inputText <- Text.IO.readFile inputFile

        (_, parsedInput) <- Core.throws (Parser.exprAndHeaderFromText mempty inputText)

        let denotedInput = Core.denote parsedInput

        let actualExpression = Core.pruneUselessRecordTrees denotedInput

        let actualText = format actualExpression

        expectedText <- Text.IO.readFile outputFile

        Tasty.HUnit.assertEqual "The pruned expression did not match the expected output" expectedText actualText
