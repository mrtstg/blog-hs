{-# LANGUAGE QuasiQuotes #-}

module Parser.Html
  ( markdownToWidget
  ) where

import           Data.Maybe   (isJust)
import           Parser.Types
import           Yesod.Core

markdownToWidget :: [MarkdownBlock] -> WidgetFor site ()
markdownToWidget blocks = [whamlet|^{mconcat $ map blockToWidget blocks}|]

blockToWidget :: MarkdownBlock -> WidgetFor site ()
blockToWidget (Paragraph lst) = [whamlet|<p> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 1 lst) = [whamlet|<h1> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 2 lst) = [whamlet|<h2> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 3 lst) = [whamlet|<h3> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 4 lst) = [whamlet|<h4> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 5 lst) = [whamlet|<h5> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header 6 lst) = [whamlet|<h6> ^{markdownInlinesToString lst}|]
blockToWidget (Parser.Types.Header _ _) = error "Unacceptable header value!"
blockToWidget (Quote lst) = [whamlet|<blockquote> ^{markdownInlinesToString lst}|]
blockToWidget (List OrderedList lst) =
  [whamlet|
<ol>
  $forall item <- lst
    <li> ^{markdownInlinesToString item}
|]
blockToWidget (List UnorderedList lst) =
  [whamlet|
<ul>
  $forall item <- lst
    <li> ^{markdownInlinesToString item}
|]
blockToWidget (Code lang code) =
  [whamlet|
<pre>
  <code :hasLang:.#{unwrappedLang}>
    #{code}
|]
  where
    hasLang = isJust lang
    unwrappedLang =
      case lang of
        (Just v) -> v
        Nothing  -> ""

markdownInlinesToString :: [MarkdownInline] -> WidgetFor site ()
markdownInlinesToString = mconcat . map markdownInlineToString

markdownInlineToString :: MarkdownInline -> WidgetFor site ()
markdownInlineToString (AbsoluteLink name href) = [whamlet|<a href=#{href}>#{name}</a>|]
markdownInlineToString (Text text) = [whamlet|#{text}|]
markdownInlineToString (Image name href) = [whamlet|<img src=#{href} alt="#{name}">|]
markdownInlineToString (Italic texts) = [whamlet|<i>^{markdownInlinesToString texts}</i>|]
markdownInlineToString (Bold texts) = [whamlet|<b>^{markdownInlinesToString texts}</b>|]
