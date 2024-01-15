{-# LANGUAGE OverloadedStrings #-}
module App.UtilsSpec (spec) where
import           App.Utils  (generatePostUrlFromRelativeFile,
                             processPostPathParts)
import           Test.Hspec

spec :: Spec
spec = do
    describe "Post URL paths processing for metadata test" $ do
        it "Cutting index at the end test" $ do
            processPostPathParts ["index"] `shouldBe` ""
            processPostPathParts ["postFolder", "index"] `shouldBe` "postFolder/"
        it "Middle index is not getting cutted out" $ do
            processPostPathParts ["foldersWithPosts", "index", "postName"] `shouldBe` "foldersWithPosts/index/postName/"
            processPostPathParts ["index", "indexPage"] `shouldBe` "index/indexPage/"
        it "Non-index parts works as usual" $ do
            processPostPathParts ["folder1", "folder2", "folder3", "post"] `shouldBe` "folder1/folder2/folder3/post/"
            processPostPathParts [] `shouldBe` ""
    describe "URL generation from relative path test" $ do
        it "Base tests" $ do
            generatePostUrlFromRelativeFile "http://example.com" "post.md" `shouldBe` "http://example.com/post/post/"
        it "Tests with index file" $ do
            generatePostUrlFromRelativeFile "https://site.url" "folder/posts/index.md" `shouldBe` "https://site.url/post/folder/posts/"

