{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Minify (getTests) where

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

minifyDirectory :: FilePath
minifyDirectory = "./tests/Dhall/Test/Minify"

getTests :: IO TestTree
getTests = do
    tests <- Test.Util.discover (Turtle.chars <* ".in.dhall") test (Turtle.lstree minifyDirectory)

    let testTree = Tasty.testGroup "Minify" [ tests ]

    return testTree

format :: Core.Expr Void Core.Import -> Text
format expr =
    let renotedExpr = Core.renote expr :: Core.Expr Parser.Src Core.Import
        doc         = Pretty.prettyCharacterSet Pretty.Unicode renotedExpr <> "\n"
        docStream   = Pretty.layout doc
    in
        Doc.Render.Text.renderStrict docStream

test :: Text -> TestTree
test prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> ".in.dhall")
        let outputFile = Text.unpack (prefix <> ".out.dhall")

        inputText <- Text.IO.readFile inputFile

        (_, parsedInput) <- Core.throws (Parser.exprAndHeaderFromText mempty inputText)

        let denotedInput = Core.denote parsedInput

        let actualExpression = Core.minify denotedInput

        let actualText = format actualExpression

        expectedText <- Text.IO.readFile outputFile

        Tasty.HUnit.assertEqual "The minified expression did not match the expected output" expectedText actualText
