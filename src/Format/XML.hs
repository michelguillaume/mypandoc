{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- XML formatter
-}

module Format.XML (renderXML) where

import           AST              (Document(..), Header(..), Block(..), Inline(..))

renderXML :: Document -> String
renderXML (Document hdr blocks) =
     "<document>\n"
  ++ renderHeader 1 hdr
  ++ renderBody   1 blocks
  ++ "</document>"

renderHeader :: Int -> Header -> String
renderHeader lvl (Header title mA mD) =
  case (mA, mD) of
    (Nothing, Nothing) ->
      indent lvl ("<header title=\"" ++ title ++ "\"></header>\n")
    _ ->
      indent lvl ("<header title=\"" ++ title ++ "\">\n")
      ++ maybe "" (\a -> indent (lvl+1) ("<author>" ++ a ++ "</author>\n")) mA
      ++ maybe "" (\d -> indent (lvl+1) ("<date>" ++ d ++ "</date>\n")) mD
      ++ indent lvl "</header>\n"

renderBody :: Int -> [Block] -> String
renderBody lvl bs =
     indent lvl "<body>\n"
  ++ concatMap (renderBlock (lvl+1)) bs
  ++ indent lvl "</body>\n"

renderBlock :: Int -> Block -> String
renderBlock lvl blk = case blk of
  Paragraph inls     -> renderParagraph lvl inls
  CodeBlock blks     -> renderCodeBlock lvl blks
  List items         -> renderList lvl items
  Section t subs     -> renderSection lvl t subs
  Raw s              -> indent lvl (s ++ "\n")

renderParagraph :: Int -> [Inline] -> String
renderParagraph lvl inls =
  indent lvl ("<paragraph>" ++ concatMap renderInline inls ++ "</paragraph>\n")

renderCodeBlock :: Int -> [Block] -> String
renderCodeBlock lvl blks =
     indent lvl "<codeblock>\n"
  ++ concatMap renderInner blks
  ++ indent lvl "</codeblock>\n"
  where
    renderInner b = case b of
      Paragraph inls ->
        indent (lvl+1) ("<paragraph>" ++
        concatMap renderInline inls ++ "</paragraph>\n")
      _ -> ""

renderList :: Int -> [[Block]] -> String
renderList lvl items =
     indent lvl "<list>\n"
  ++ concatMap (concatMap (renderBlock (lvl+1))) items
  ++ indent lvl "</list>\n"

renderSection :: Int -> String -> [Block] -> String
renderSection lvl title subs =
     indent lvl ("<section title=\"" ++ title ++ "\">\n")
  ++ concatMap (renderBlock (lvl+1)) subs
  ++ indent lvl "</section>\n"

renderInline :: Inline -> String
renderInline inl = case inl of
  Plain s      -> s
  Bold xs      -> "<bold>"  ++ concatMap renderInline xs ++ "</bold>"
  Italic xs    -> "<italic>"++ concatMap renderInline xs ++ "</italic>"
  CodeSpan s   -> "<code>"  ++ s                     ++ "</code>"
  Link  u xs   ->
    "<link url=\"" ++ u ++ "\">" ++ concatMap renderInline xs ++ "</link>"
  Image u xs   ->
    "<image url=\"" ++ u ++ "\">" ++ concatMap renderInline xs ++ "</image>"

indent :: Int -> String -> String
indent n str = replicate (n * 4) ' ' ++ str
