{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- XML renderer
-}

module Format.XML (renderXML) where

import AST (Document(..), Header(..), Block(..), Inline(..))

-- | Render a Document back to XML with 4-space indentation
renderXML :: Document -> String
renderXML (Document hdr blocks) =
     "<document>\n"
  ++ renderHeader 1 hdr
  ++ renderBody   1 blocks
  ++ "</document>\n"

-- | Render the <header> (title + optional author/date)
renderHeader :: Int -> Header -> String

renderHeader lvl (Header title Nothing Nothing) =
    indent lvl ("<header title=\"" ++ title ++ "\"></header>\n")

renderHeader lvl (Header title mAuthor mDate) =
     indent lvl ("<header title=\"" ++ title ++ "\">\n")
  ++ maybe "" (\a -> indent (lvl+1) ("<author>" ++ a ++ "</author>\n")) mAuthor
  ++ maybe "" (\d -> indent (lvl+1) ("<date>"   ++ d ++ "</date>\n"))   mDate
  ++ indent lvl "</header>\n"


-- | Render the <body> and its child blocks
renderBody :: Int -> [Block] -> String
renderBody lvl blocks =
     indent lvl "<body>\n"
  ++ concatMap (renderBlock (lvl+1)) blocks
  ++ indent lvl "</body>\n"

-- | Render any Block
renderBlock :: Int -> Block -> String
renderBlock lvl blk = case blk of
  Paragraph inls     -> renderParagraph lvl inls
  CodeBlock codes    -> renderCodeBlock lvl codes
  List items         -> renderList lvl items
  Section title subs -> renderSection lvl title subs

-- | Paragraph
renderParagraph :: Int -> [Inline] -> String
renderParagraph lvl inls =
  indent lvl ("<paragraph>" ++ concatMap renderInline inls ++ "</paragraph>\n")

-- | CodeBlock
renderCodeBlock :: Int -> [String] -> String
renderCodeBlock lvl codes =
     indent lvl "<codeblock>\n"
  ++ concatMap
       (\c -> indent (lvl+1) ("<paragraph>"++c++"</paragraph>\n"))
       codes
  ++ indent lvl "</codeblock>\n"

-- | List
renderList :: Int -> [[Inline]] -> String
renderList lvl items =
     indent lvl "<list>\n"
  ++ concatMap
       (\inls -> indent (lvl+1)
         ("<paragraph>"++concatMap renderInline inls++"</paragraph>\n"))
       items
  ++ indent lvl "</list>\n"

-- | Section
renderSection :: Int -> String -> [Block] -> String
renderSection lvl title bs =
     indent lvl ("<section title=\""++title++"\">\n")
  ++ concatMap (renderBlock (lvl+1)) bs
  ++ indent lvl "</section>\n"

-- | Render inline content inside a paragraph
renderInline :: Inline -> String
renderInline inl =
  case inl of
    Plain s       -> s
    Bold xs       -> "<bold>"  ++ concatMap renderInline xs ++ "</bold>"
    Italic xs     -> "<italic>"++ concatMap renderInline xs ++ "</italic>"
    CodeSpan s    -> "<code>"  ++ s                   ++ "</code>"
    Link url xs   -> "<link url=\"" ++ url ++ "\">"
                   ++ concatMap renderInline xs ++ "</link>"
    Image url xs  -> "<image url=\"" ++ url ++ "\">"
                   ++ concatMap renderInline xs ++ "</image>"

-- | Indent a line by n levels of 4 spaces
indent :: Int -> String -> String
indent n str = replicate (n * 4) ' ' ++ str
