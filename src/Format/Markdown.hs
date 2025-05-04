{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- Markdown formatter
-}

module Format.Markdown (renderMarkdown) where

import           AST                (Document(..), Header(..), Block(..), Inline(..))
import           Data.Maybe         (maybeToList)
import           Data.List          (intercalate)

renderMarkdown :: Document -> String
renderMarkdown (Document hdr body) =
  let front    = renderFrontMatter hdr
      spacer   = if null body then ""  else "\n"
      bodyText = renderBlocks 0 body
      trailing = if null body then "\n" else "\n"
  in front ++ spacer ++ bodyText ++ trailing

renderFrontMatter :: Header -> String
renderFrontMatter (Header title mA mD) =
  unlines $
    [ "---"
    , "title: " ++ title
    ]
    ++ ["author: " ++ a | a <- maybeToList mA]
    ++ ["date: "   ++ d | d <- maybeToList mD]
    ++ ["---"]

renderBlocks :: Int -> [Block] -> String
renderBlocks _   []         = ""
renderBlocks lvl [b]        = renderBlock lvl b
renderBlocks lvl (b1:b2:bs) =
  renderBlock lvl b1
  ++ sepBetween b1 b2
  ++ renderBlocks lvl (b2:bs)

renderBlock :: Int -> Block -> String
renderBlock lvl blk =
  case blk of
    Paragraph inls      -> renderParagraph inls
    CodeBlock subBlks   -> renderCodeBlock subBlks
    List items          -> renderList items
    Section "" subs     -> renderAnonymousSection lvl subs
    Section title subs  -> renderSection lvl title subs
    Raw s               -> renderRaw s

renderParagraph :: [Inline] -> String
renderParagraph = renderInlines

renderCodeBlock :: [Block] -> String
renderCodeBlock subBlks =
  let codeLines = [ concatMap inlineText inls | Paragraph inls <- subBlks ]
  in  "```\n" ++ unlines codeLines ++ "```"

renderList :: [[Block]] -> String
renderList items =
  intercalate "\n"
    [ "- " ++ renderBlock 0 blk
    | blkList <- items
    , blk     <- blkList
    ]

renderAnonymousSection :: Int -> [Block] -> String
renderAnonymousSection lvl subs =
  renderBlocks (lvl + 1) subs

renderSection :: Int -> String -> [Block] -> String
renderSection lvl title subs =
  let hashes     = replicate (lvl + 1) '#'
      headerLine = hashes ++ " " ++ title ++ "\n\n"
      content    = renderBlocks (lvl + 1) subs
  in headerLine ++ content

renderRaw :: String -> String
renderRaw = id

sepBetween :: Block -> Block -> String
sepBetween (Paragraph inls) next
  | any isMediaInline inls =
      case next of
        Paragraph inls2 | any isMediaInline inls2 -> "\n"
        _                                        -> "\n\n"
sepBetween (CodeBlock _)   _ = "\n"
sepBetween (List _)        _ = "\n\n"
sepBetween _               _ = "\n\n"

renderInlines :: [Inline] -> String
renderInlines = concatMap renderInline

renderInline :: Inline -> String
renderInline (Plain s)    = s
renderInline (Bold xs)    = "**" ++ renderInlines xs ++ "**"
renderInline (Italic xs)  = "*"  ++ renderInlines xs ++ "*"
renderInline (CodeSpan s) = "`"  ++ s             ++ "`"
renderInline (Link u xs)  = "[" ++ renderInlines xs ++ "](" ++ u ++ ")"
renderInline (Image u xs) = "![" ++ renderInlines xs ++ "](" ++ u ++ ")"

isMediaInline :: Inline -> Bool
isMediaInline (Link _ _)  = True
isMediaInline (Image _ _) = True
isMediaInline _           = False

inlineText :: Inline -> String
inlineText (Plain s)      = s
inlineText (Bold xs)      = concatMap inlineText xs
inlineText (Italic xs)    = concatMap inlineText xs
inlineText (CodeSpan s)   = s
inlineText (Link _ xs)    = concatMap inlineText xs
inlineText (Image _ xs)   = concatMap inlineText xs
