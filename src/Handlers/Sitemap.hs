{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handlers.Sitemap (getSitemapR) where

import           App.Config              (disabledPages, siteHost)
import           App.Config.PageSettings
import           App.Utils               (generatePostUrlFromRelativeFile)
import           Crud                    (selectAllPosts)
import qualified Data.ByteString         as B
import           Data.Function           ((&))
import qualified Data.Text               as T
import           Database.Persist
import           Foundation
import           Text.XML.Generator
import           Yesod.Core
import           Yesod.Persist

generateXML :: [Entity Post] -> String -> B.ByteString
generateXML posts baseHost = let
    urlProcessFunction = generatePostUrlFromRelativeFile baseHost
    in xrender $ doc defaultDocInfo $
    xelem "urlset" $
        xattr "xlmns" "http://www.sitemaps.org/schemas/sitemap/0.9" <#>
            xelems (map (\(Entity _ Post { postFile = file, postDate = date }) ->
                xelem "url" (xelems [xelem "loc" ((xtext . T.pack . urlProcessFunction) file), xelem "lastmod" ((xtext . T.pack . show) date), xelem "changefreq" (xtext "weekly")])) posts)

getSitemapR :: Handler TypedContent
getSitemapR = do
    App { .. } <- getYesod
    let (PageSettings disabledPages') = disabledPages config
    if SitemapPage `elem` disabledPages' then notFound else do
      case config & siteHost of
        Nothing -> notFound
        (Just host) -> do
          posts <- runDB selectAllPosts
          return $ TypedContent "application/xml" (toContent $ generateXML posts host)
