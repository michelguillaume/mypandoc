{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- Markdown renderer
-}

module Format.Markdown (renderMarkdown) where

import Data.List (intercalate)
import AST (Document(..), Header(..), Block(..), Inline(..))
import Data.Maybe (maybeToList)

-- | Render an entire Document as Markdown, with trailing newline
renderMarkdown :: Document -> String
renderMarkdown (Document hdr body) =
  renderFrontMatter hdr
  ++ renderBlocks 0 body
  ++ "\n"

-- | YAML front-matter
renderFrontMatter :: Header -> String
renderFrontMatter (Header title mAuth mDate) =
  unlines $
    [ "---"
    , "title: " ++ title
    ]
    ++ [ "author: " ++ a | a <- maybeToList mAuth ]
    ++ [ "date: "   ++ d | d <- maybeToList mDate ]
    ++ [ "---", "" ]

-- | Render a sequence of blocks with lookahead on consecutive blocks
renderBlocks :: Int -> [Block] -> String
renderBlocks _   []        = ""
renderBlocks lvl [b]       = renderBlock lvl b
renderBlocks lvl (b1:b2:bs) =
  renderBlock lvl b1
  ++ sepBetween b1 b2
  ++ renderBlocks lvl (b2:bs)

-- | Render a single block without trailing newline
renderBlock :: Int -> Block -> String
renderBlock _   (Paragraph inls)   = renderInlines inls
renderBlock _   (CodeBlock ls)     = "```\n" ++ unlines ls ++ "```"
renderBlock _   (List items)       =
  intercalate "\n" [ "- " ++ renderInlines item | item <- items ]
renderBlock lvl (Section "" subs) = renderBlocks (lvl+1) subs
renderBlock lvl (Section title subs) =
  let hashes     = replicate (lvl+1) '#'
      headerLine = hashes ++ " " ++ title ++ "\n\n"
      content    = renderBlocks (lvl+1) subs
  in headerLine ++ content

-- | Separator between two blocks
sepBetween :: Block -> Block -> String
sepBetween (Paragraph inls) next
  | any isMediaInline inls =
      case next of
        Paragraph inls2 | any isMediaInline inls2 -> "\n"
        _                                          -> "\n\n"
sepBetween (CodeBlock _)      _               = "\n"
sepBetween (List _)           _               = "\n\n"
sepBetween _                  _               = "\n\n"

-- | Detect if inline is a media (link or image)
isMediaInline :: Inline -> Bool
isMediaInline (Link _ _)  = True
isMediaInline (Image _ _) = True
isMediaInline _           = False

-- | Render inline elements
renderInlines :: [Inline] -> String
renderInlines = concatMap renderInline

renderInline :: Inline -> String
renderInline (Plain s)     = s
renderInline (Bold xs)     = "**" ++ renderInlines xs ++ "**"
renderInline (Italic xs)   = "*"  ++ renderInlines xs ++ "*"
renderInline (CodeSpan s)  = "`"  ++ s             ++ "`"
renderInline (Link url xs) = "["  ++ renderInlines xs ++ "](" ++ url ++ ")"
renderInline (Image url xs)= "![" ++ renderInlines xs ++ "](" ++ url ++ ")"
