{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import           Data.Attoparsec.Text
import           Data.Either
import           Data.Text
import           Parser               (parseMarkdown)
import           Parser.Block
import           Parser.Types
import           Parser.Utils
import           Test.Hspec

mdExtendedTable :: Text
mdExtendedTable = "| Column 1 | Column 2 |\n|----------|----------|\n| Value 1 | Value 2 |\n"

mdExtendedTableMultiple :: Text
mdExtendedTableMultiple = "| Column 1 | Column 2 | Column 3 |\n|-|-|-|\n| 1-1 | 1-2 | 1-3 |\n| Data 1 | Data 2 | Data 3|\n|Last cell 1|    Last cell 2|Last cell 3|\n"

formattedTable :: Text
formattedTable = "| Column 1 | *Column 2* |\n|-|-|\n|**Content**| [Link](google.com) |\n"

spec :: Spec
spec = do
  describe "Table parsing" $ do
    it "Table content parse" $ do
      parseOnly parseTableContent "| Col1 | Col2 |\n" `shouldBe` Right ["Col1 ", "Col2 "]
      parseOnly parseTableContent "|Col1|Col2|\n" `shouldBe` Right ["Col1", "Col2"]
      parseOnly parseTableContent "|Col1|Col2|" `shouldBe` Right ["Col1", "Col2"]
      parseOnly parseTableContent "| Col1 | Col2 \n" `shouldSatisfy` isLeft
    it "Table divider parse" $ do
      parseOnly tableDivider "|------|-------|\n" `shouldSatisfy` isRight
      parseOnly tableDivider "|--------------|\n" `shouldSatisfy` isRight
      parseOnly tableDivider "---------------\n" `shouldSatisfy` isRight
    it "Markdown extended syntax example" $ do
      parseOnly parseTable mdExtendedTable `shouldBe` Right (Table [[Text "Column 1"], [Text "Column 2"]] [[[Text "Value 1"], [Text "Value 2"]]])
      parseOnly parseTable mdExtendedTableMultiple `shouldBe` Right
        (Table [[Text "Column 1"], [Text "Column 2"], [Text "Column 3"]]
        [[[Text "1-1"], [Text "1-2"], [Text "1-3"]], [[Text "Data 1"], [Text "Data 2"], [Text "Data 3"]], [[Text "Last cell 1"], [Text "Last cell 2"], [Text "Last cell 3"]]]
        )
    it "Formatted table text" $ do
      parseOnly parseTable formattedTable `shouldBe` Right (Table [[Text "Column 1"], [Italic [Text "Column 2"]]] [[[Bold [Text "Content"]], [AbsoluteLink "Link" "google.com"]]])
