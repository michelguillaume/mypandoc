{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ParserMarkdownSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Parser.Core (runParser)
import Parser.Markdown (parseMarkdownDocument)
import AST (Document(..), Header(..), Block(..), Inline(..))

runP :: String -> Maybe (Document, String)
runP = runParser parseMarkdownDocument

spec :: Spec
spec = describe "Parser.Markdown.parseMarkdownDocument (input → expected Document)" $ do
  let testHeader t = unlines
        [ "---"
        , "title: " ++ t
        , "---"
        , ""
        ]

      staticCases :: [(String, String, Document)]
      staticCases =
        [ ("01. raw seul"
          , testHeader "T1" ++ "foo\n"
          , Document (Header "T1" Nothing Nothing)
                     [Paragraph [Plain "foo"]]
          )

        , ("02. paragraphe 1 mot"
          , testHeader "T2" ++ "bar\n"
          , Document (Header "T2" Nothing Nothing)
                     [Paragraph [Plain "bar"]]
          )

        , ("03. paragraphe 2 mots"
          , testHeader "T3" ++ "a b\n"
          , Document (Header "T3" Nothing Nothing)
                     [Paragraph [Plain "a b"]]
          )

        , ("04. paragraphe 4 mots"
          , testHeader "T4" ++ "w1 w2 w3 w4\n"
          , Document (Header "T4" Nothing Nothing)
                     [Paragraph [Plain "w1 w2 w3 w4"]]
          )

        , ("05. bold seul"
          , testHeader "T5" ++ "**B**\n"
          , Document (Header "T5" Nothing Nothing)
                     [Paragraph [Bold [Plain "B"]]]
          )

        , ("06. italic seul"
          , testHeader "T6" ++ "*I*\n"
          , Document (Header "T6" Nothing Nothing)
                     [Paragraph [Italic [Plain "I"]]]
          )

        , ("07. codeSpan inline"
          , testHeader "T7" ++ "`C`\n"
          , Document (Header "T7" Nothing Nothing)
                     [Paragraph [CodeSpan "C"]]
          )

        , ("08. link simple"
          , testHeader "T8" ++ "[c](u)\n"
          , Document (Header "T8" Nothing Nothing)
                     [Paragraph [Link "u" [Plain "c"]]]
          )

        , ("09. image simple"
          , testHeader "T9" ++ "![alt](img.png)\n"
          , Document (Header "T9" Nothing Nothing)
                     [Paragraph [Image "img.png" [Plain "alt"]]]
          )

        , ("10. link + bold + texte"
          , testHeader "T10" ++ "**B** & [here](u)\n"
          , Document (Header "T10" Nothing Nothing)
                     [Paragraph [Bold [Plain "B"], Plain " & ", Link "u" [Plain "here"]]]
          )

        , ("11. codeblock raw"
          , testHeader "T11" ++ "```\nhi\n```\n"
          , Document (Header "T11" Nothing Nothing)
                     [CodeBlock [Paragraph [Plain "hi"]]]
          )

        , ("12. codeblock deux lignes"
          , testHeader "T12" ++ "```\nl1\nl2\n```\n"
          , Document (Header "T12" Nothing Nothing)
                     [CodeBlock [Paragraph [Plain "l1"], Paragraph [Plain "l2"]]]
          )

        , ("13. liste 1 item"
          , testHeader "T13" ++ "- i\n"
          , Document (Header "T13" Nothing Nothing)
                     [List [[Paragraph [Plain "i"]]]]
          )

        , ("14. liste 2 items"
          , testHeader "T14" ++ "- a\n- b\n"
          , Document (Header "T14" Nothing Nothing)
                     [List [[Paragraph [Plain "a"]],[Paragraph [Plain "b"]]]]
          )

        , ("15. liste avec codeblock"
          , testHeader "T15" ++ "- x\n```\nc\n```\n"
          , Document (Header "T15" Nothing Nothing)
                     [List [[Paragraph [Plain "x"]]], CodeBlock [Paragraph [Plain "c"]]]
          )

        , ("16. liste avec section"
          , testHeader "T16" ++ "- y\n# S\nz\n"
          , Document (Header "T16" Nothing Nothing)
                     [List [[Paragraph [Plain "y"]]], Section "S" [Paragraph [Plain "z"]]]
          )

        , ("17. nested list"
          , testHeader "T17" ++ "- i1 i2\n  - j\n"
          , Document (Header "T17" Nothing Nothing)
                     [List [[Paragraph [Plain "i1 i2"]]], List [[Paragraph [Plain "j"]]]]
          )

        , ("18. section simple"
          , testHeader "T18" ++ "# Sec\np\n"
          , Document (Header "T18" Nothing Nothing)
                     [Section "Sec" [Paragraph [Plain "p"]]]
          )

        , ("19. section avec codeblock"
          , testHeader "T19" ++ "# S2\n```\nc2\n```\n"
          , Document (Header "T19" Nothing Nothing)
                     [Section "S2" [CodeBlock [Paragraph [Plain "c2"]]]]
          )

        , ("20. section avec liste"
          , testHeader "T20" ++ "# S3\n- k\n"
          , Document (Header "T20" Nothing Nothing)
                     [Section "S3" [List [[Paragraph [Plain "k"]]]]]
          )

        , ("21. section imbriquée & mixte"
          , testHeader "T21" ++ "# Root\na\n- l1\n- l2\n## Child\nc\n"
          , Document (Header "T21" Nothing Nothing)
                     [Section "Root"
                       [ Paragraph [Plain "a"]
                       , List [[Paragraph [Plain "l1"]],[Paragraph [Plain "l2"]]]
                       , Section "Child" [Paragraph [Plain "c"]]
                       ]
                     ]
          )

        , ("22. doc mixte plusieurs blocks"
          , testHeader "T22"
            ++ "raw\n\np1 p2\n```\ncb\n```\n- i\n# S4\nz\n"
          , Document (Header "T22" Nothing Nothing)
                     [ Paragraph [Plain "raw"]
                     , Paragraph [Plain "p1 p2"]
                     , CodeBlock [Paragraph [Plain "cb"]]
                     , List [[Paragraph [Plain "i"]]]
                     , Section "S4" [Paragraph [Plain "z"]]
                     ]
          )

        , ("23. empty body"
          , testHeader "TE"
          , Document (Header "TE" Nothing Nothing) []
          )

        , ("24. header author+date"
          , unlines ["---","title: H","author: Au","date: D","---",""]
          , Document (Header "H" (Just "Au") (Just "D")) []
          )

        , ("25. header date only"
          , unlines ["---","title: HD","date: 2025-01-01","---",""]
          , Document (Header "HD" Nothing (Just "2025-01-01")) []
          )

        , ("26. nested bold in italic"
          , testHeader "T26" ++ "* **B** *\n"
          , Document (Header "T26" Nothing Nothing)
                     [Paragraph [Italic [Plain " "], Italic [Plain "B"], Italic [Plain " "]]]
          )

        , ("27. nested italic in bold"
          , testHeader "T27" ++ "** *I* **\n"
          , Document (Header "T27" Nothing Nothing)
                     [Paragraph [Bold [Plain " ", Italic [Plain "I"], Plain " "]]]
          )

        , ("28. mixed inline all types"
          , testHeader "T28" ++ "start**B***I*`C`[L](u)![X](i)end\n"
          , Document (Header "T28" Nothing Nothing)
                     [Paragraph [Plain "start",Bold [Plain "B"],Italic [Plain "I"],CodeSpan "C",Link "u" [Plain "L"],Image "i" [Plain "X"],Plain "end"]]
          )

        , ("29. list item with multiple inlines"
          , testHeader "T29" ++ "- a`c`b\n"
          , Document (Header "T29" Nothing Nothing)
                     [List [[Paragraph [Plain "a",CodeSpan "c",Plain "b"]]]]
          )

        , ("30. deeply nested lists"
          , testHeader "T30" ++ "- - x\n  - - y\n"
          , Document (Header "T30" Nothing Nothing)
                     [List [[List [[Paragraph [Plain "x"]]]],[List [[List [[Paragraph [Plain "y"]]]]]]]]
          )

        , ("31. nested codeblock object"
          , testHeader "T31" ++ "```\n```inner```\n```\n"
          , Document (Header "T31" Nothing Nothing)
                     [CodeBlock [CodeBlock [Paragraph [Plain "inner"]]]]
          )

        , ("32. section with mixed content"
          , testHeader "T32"
            ++ "# Mix\nt\n**B**\n```\nC\n```\n- L\n"
          , Document (Header "T32" Nothing Nothing)
                     [Section "Mix"
                       [ Paragraph [Plain "t"]
                       , Paragraph [Bold [Plain "B"]]
                       , CodeBlock [Paragraph [Plain "C"]]
                       , List [[Paragraph [Plain "L"]]]
                       ]
                     ]
          )

        , ("33. section in list in section"
          , testHeader "T33"
            ++ "# Top\n- z\n  ## Inner\n  x\n"
          , Document (Header "T33" Nothing Nothing)
                     [Section "Top"
                       [ List [[Paragraph [Plain "z"]]]
                       , Section "Inner" [Paragraph [Plain "x"]]
                       ]
                     ]
          )

        , ("34. image alt nested inline"
          , testHeader "T34" ++ "![**B** *I*](i)\n"
          , Document (Header "T34" Nothing Nothing)
                     [Paragraph [Image "i" [Bold [Plain "B"],Plain " ", Italic [Plain "I"]]]]
          )

        , ("35. link alt nested inline"
          , testHeader "T35" ++ "[go *i*](u)\n"
          , Document (Header "T35" Nothing Nothing)
                     [Paragraph [Link "u" [Plain "go ",Italic [Plain "i"]]]]
          )

        , ("36. list item string simple = Paragraph"
          , testHeader "T36" ++ "- item\n"
          , Document (Header "T36" Nothing Nothing)
                     [List [[Paragraph [Plain "item"]]]]
          )

        , ("37. list item = codeblock"
          , testHeader "T37" ++ "- ```\ncb\n```\n"
          , Document (Header "T37" Nothing Nothing)
                     [List [[CodeBlock [Paragraph [Plain "cb"]]]]]
          )

        , ("38. codeblock deux lignes string = Raw concat"
          , testHeader "T38" ++ "```\nl1\nl2\n```\n"
          , Document (Header "T38" Nothing Nothing)
                     [CodeBlock [Paragraph [Plain "l1"],Paragraph [Plain "l2"]]]
          )

        , ( "39. codeblock = raw + paragraph"
        , testHeader "T39" ++ unlines
            [ "```"
            , "raw"
            , ""
            , "paragraph"
            , "```"
            ]
        , Document (Header "T39" Nothing Nothing)
            [ CodeBlock
                [ Paragraph [Plain "raw"]
                , Paragraph [Plain ""]
                , Paragraph [Plain "paragraph"]
                ]
            ]
        )

        , ("40. section vide"
          , testHeader "T40" ++ "# Empty\n"
          , Document (Header "T40" Nothing Nothing)
                     [Section "Empty" []]
          )

        , ("41. paragraph vide"
          , testHeader "T41" ++ "\n"
          , Document (Header "T41" Nothing Nothing)
                     []
          )

        , ("42. image alt vide"
          , testHeader "T42" ++ "![](img.png)\n"
          , Document (Header "T42" Nothing Nothing)
                     [Paragraph [Image "img.png" []]]
          )

        , ("43. link content vide"
          , testHeader "T43" ++ "[](u)\n"
          , Document (Header "T43" Nothing Nothing)
                     [Paragraph [Link "u" []]]
          )

        , ("44. list vide"
          , testHeader "T44" ++ "- \n"
          , Document (Header "T44" Nothing Nothing)
                     [List [[Paragraph []]]]
          )

        , ("45. raw string vide"
          , testHeader "T45" ++ "\n"
          , Document (Header "T45" Nothing Nothing)
                     []
          )

        , ("46. inline string vide"
          , testHeader "T46" ++ "* *\n"
          , Document (Header "T46" Nothing Nothing)
                     [Paragraph [Italic [Plain " "]]]
          )

        , ("47. list avec Raw vide"
          , testHeader "T47" ++ "- \n"
          , Document (Header "T47" Nothing Nothing)
                     [List [[Paragraph []]]]
          )

        , ("48. deeply nested inline"
          , testHeader "T48" ++ "***`deep`***\n"
          , Document (Header "T48" Nothing Nothing)
                     [Paragraph [Italic [Bold [CodeSpan "deep"]]]]
          )

        , ("49. section avec raw + codeblock"
          , testHeader "T49" ++ "# S\nt\n```c```\n"
          , Document (Header "T49" Nothing Nothing)
                     [Section "S" [Paragraph [Plain "t"],CodeBlock [Paragraph [Plain "c"]]]]
          )

        , ("50. paragraph tous inlines"
          , testHeader "T50" ++ "t**b***i*`c`[l](u)![a](img)\n"
          , Document (Header "T50" Nothing Nothing)
                     [Paragraph [Plain "t",Bold [Plain "b"],Italic [Plain "i"],CodeSpan "c",Link "u" [Plain "l"],Image "img" [Plain "a"]]]
          )
        ]

  forM_ staticCases $ \(name, input, expected) ->
    it name $ runP input `shouldBe` Just (expected, "")
