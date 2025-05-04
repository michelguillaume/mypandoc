{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ParserXmlSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Parser.Core (runParser)
import Parser.XML (parseXMLDocument)
import AST (Document(..), Header(..), Block(..), Inline(..))

runP :: String -> Maybe (Document, String)
runP = runParser parseXMLDocument

spec :: Spec
spec = describe "Parser.XML.parseXMLDocument (input → expected Document)" $ do

  let staticCases :: [(String, String, Document)]
      staticCases =
        [ ("01. raw seul",
           "<document><header title=\"T1\"></header><body>foo</body></document>",
           Document (Header "T1" Nothing Nothing) [Raw "foo"]),

          ("02. paragraphe 1 mot",
           "<document><header title=\"T2\"></header><body><paragraph>bar</paragraph></body></document>",
           Document (Header "T2" Nothing Nothing) [Paragraph [Plain "bar"]]),

          ("03. paragraphe 2 mots",
           "<document><header title=\"T3\"></header><body><paragraph>a b</paragraph></body></document>",
           Document (Header "T3" Nothing Nothing) [Paragraph [Plain "a b"]]),

          ("04. paragraphe 4 mots",
           "<document><header title=\"T4\"></header><body><paragraph>w1 w2 w3 w4</paragraph></body></document>",
           Document (Header "T4" Nothing Nothing) [Paragraph [Plain "w1 w2 w3 w4"]]),

          ("05. bold seul",
           "<document><header title=\"T5\"></header><body><paragraph><bold>B</bold></paragraph></body></document>",
           Document (Header "T5" Nothing Nothing) [Paragraph [Bold [Plain "B"]]]),

          ("06. italic seul",
           "<document><header title=\"T6\"></header><body><paragraph><italic>I</italic></paragraph></body></document>",
           Document (Header "T6" Nothing Nothing) [Paragraph [Italic [Plain "I"]]]),

          ("07. codeSpan inline",
           "<document><header title=\"T7\"></header><body><paragraph><code>C</code></paragraph></body></document>",
           Document (Header "T7" Nothing Nothing) [Paragraph [CodeSpan "C"]]),

          ("08. link simple",
           "<document><header title=\"T8\"></header><body><paragraph><link url=\"u\">c</link></paragraph></body></document>",
           Document (Header "T8" Nothing Nothing) [Paragraph [Link "u" [Plain "c"]]]),

          ("09. image simple",
           "<document><header title=\"T9\"></header><body><paragraph><image url=\"img.png\">alt</image></paragraph></body></document>",
           Document (Header "T9" Nothing Nothing) [Paragraph [Image "img.png" [Plain "alt"]]]),

          ("10. link + bold + texte",
           "<document><header title=\"T10\"></header><body><paragraph><bold>B</bold> & <link url=\"u\">here</link></paragraph></body></document>",
           Document (Header "T10" Nothing Nothing) [Paragraph [Bold [Plain "B"], Plain " & ", Link "u" [Plain "here"]]]),

          ("11. codeblock raw",
           "<document><header title=\"T11\"></header><body><codeblock>hi</codeblock></body></document>",
           Document (Header "T11" Nothing Nothing) [CodeBlock [Raw "hi"]]),

          ("12. codeblock deux lignes",
           "<document><header title=\"T12\"></header><body><codeblock>l1l2</codeblock></body></document>",
           Document (Header "T12" Nothing Nothing) [CodeBlock [Raw "l1l2"]]),

          ("13. liste 1 item",
           "<document><header title=\"T13\"></header><body><list><paragraph>i</paragraph></list></body></document>",
           Document (Header "T13" Nothing Nothing) [List [[Paragraph [Plain "i"]]]]),

          ("14. liste 2 items",
           "<document><header title=\"T14\"></header><body><list><paragraph>a</paragraph><paragraph>b</paragraph></list></body></document>",
           Document (Header "T14" Nothing Nothing) [List [[Paragraph [Plain "a"]],[Paragraph [Plain "b"]]]]),

          ("15. liste avec codeblock",
           "<document><header title=\"T15\"></header><body><list><paragraph>x</paragraph><codeblock>c</codeblock></list></body></document>",
           Document (Header "T15" Nothing Nothing) [List [[Paragraph [Plain "x"]],[CodeBlock [Raw "c"]]]]),

          ("16. liste avec section",
           "<document><header title=\"T16\"></header><body><list><paragraph>y</paragraph><section title=\"S\"><paragraph>z</paragraph></section></list></body></document>",
           Document (Header "T16" Nothing Nothing) [List [[Paragraph [Plain "y"]],[Section "S" [Paragraph [Plain "z"]]]]]),

          ("17. nested list",
           "<document><header title=\"T17\"></header><body><list><paragraph>i1 i2</paragraph><list><paragraph>j</paragraph></list></list></body></document>",
           Document (Header "T17" Nothing Nothing) [List [[Paragraph [Plain "i1 i2"]],[List [[Paragraph [Plain "j"]]]]]]),

          ("18. section simple",
           "<document><header title=\"T18\"></header><body><section title=\"Sec\"><paragraph>p</paragraph></section></body></document>",
           Document (Header "T18" Nothing Nothing) [Section "Sec" [Paragraph [Plain "p"]]]),

          ("19. section avec codeblock",
           "<document><header title=\"T19\"></header><body><section title=\"S2\"><codeblock>c2</codeblock></section></body></document>",
           Document (Header "T19" Nothing Nothing) [Section "S2" [CodeBlock [Raw "c2"]]]),

          ("20. section avec liste",
           "<document><header title=\"T20\"></header><body><section title=\"S3\"><list><paragraph>k</paragraph></list></section></body></document>",
           Document (Header "T20" Nothing Nothing) [Section "S3" [List [[Paragraph [Plain "k"]]]]]),

          ("21. section imbriquée & mixte",
           "<document><header title=\"T21\"></header><body><section title=\"Root\"><paragraph>a</paragraph><list><paragraph>l1</paragraph><paragraph>l2</paragraph></list><section title=\"Child\"><paragraph>c</paragraph></section></section></body></document>",
           Document (Header "T21" Nothing Nothing)
                    [ Section "Root"
                        [ Paragraph [Plain "a"]
                        , List [[Paragraph [Plain "l1"]],[Paragraph [Plain "l2"]]]
                        , Section "Child" [Paragraph [Plain "c"]]
                        ]]),

          ("22. doc mixte plusieurs blocks",
           "<document><header title=\"T22\"></header><body>raw<paragraph>p1 p2</paragraph><codeblock>cb</codeblock><list><paragraph>i</paragraph></list><section title=\"S4\"><paragraph>z</paragraph></section></body></document>",
           Document (Header "T22" Nothing Nothing)
                    [ Raw "raw"
                    , Paragraph [Plain "p1 p2"]
                    , CodeBlock [Raw "cb"]
                    , List [[Paragraph [Plain "i"]]]
                    , Section "S4" [Paragraph [Plain "z"]]
                    ]),

          ("23. empty body",
           "<document><header title=\"TE\"></header><body></body></document>",
           Document (Header "TE" Nothing Nothing) []),

          ("24. header author+date",
           "<document><header title=\"H\"><author>Au</author><date>D</date></header><body></body></document>",
           Document (Header "H" (Just "Au") (Just "D")) []),

          ("25. header date only",
           "<document><header title=\"HD\"><date>2025-01-01</date></header><body></body></document>",
           Document (Header "HD" Nothing (Just "2025-01-01")) []),

          ("26. nested bold in italic",
           "<document><header title=\"T26\"></header><body><paragraph><italic><bold>B</bold></italic></paragraph></body></document>",
           Document (Header "T26" Nothing Nothing) [Paragraph [Italic [Bold [Plain "B"]]]]),

          ("27. nested italic in bold",
           "<document><header title=\"T27\"></header><body><paragraph><bold><italic>I</italic></bold></paragraph></body></document>",
           Document (Header "T27" Nothing Nothing) [Paragraph [Bold [Italic [Plain "I"]]]]),

          ("28. mixed inline all types",
           "<document><header title=\"T28\"></header><body><paragraph>start<bold>B</bold><italic>I</italic><code>C</code><link url=\"u\">L</link><image url=\"i\">X</image>end</paragraph></body></document>",
           Document (Header "T28" Nothing Nothing)
                    [ Paragraph
                        [ Plain "start"
                        , Bold [Plain "B"]
                        , Italic [Plain "I"]
                        , CodeSpan "C"
                        , Link "u" [Plain "L"]
                        , Image "i" [Plain "X"]
                        , Plain "end"
                        ]]),

          ("29. list item with multiple inlines",
           "<document><header title=\"T29\"></header><body><list><paragraph>a<code>c</code>b</paragraph></list></body></document>",
           Document (Header "T29" Nothing Nothing) [List [[Paragraph [Plain "a",CodeSpan "c",Plain "b"]]]]),

          ("30. deeply nested lists",
           "<document><header title=\"T30\"></header><body><list><list><paragraph>x</paragraph></list><list><paragraph>y</paragraph></list></list></body></document>",
           Document (Header "T30" Nothing Nothing)
                    [ List
                        [ [ List [[Paragraph [Plain "x"]]] ]
                        , [ List [[Paragraph [Plain "y"]]] ]
                        ]]),

          ("31. nested codeblock object",
           "<document><header title=\"T31\"></header><body><codeblock><codeblock>inner</codeblock></codeblock></body></document>",
           Document (Header "T31" Nothing Nothing) [CodeBlock [CodeBlock [Raw "inner"]]]),

          ("32. section with mixed content",
           "<document><header title=\"T32\"></header><body><section title=\"Mix\">t<paragraph><bold>B</bold></paragraph><codeblock>C</codeblock><list><paragraph>L</paragraph></list></section></body></document>",
           Document (Header "T32" Nothing Nothing)
                    [ Section "Mix"
                        [ Raw "t"
                        , Paragraph [Bold [Plain "B"]]
                        , CodeBlock [Raw "C"]
                        , List [[Paragraph [Plain "L"]]]
                        ]]),

          ("33. section in list in section",
           "<document><header title=\"T33\"></header><body><section title=\"Top\"><list><paragraph>z</paragraph><section title=\"Inner\"><paragraph>x</paragraph></section></list></section></body></document>",
           Document (Header "T33" Nothing Nothing)
                    [ Section "Top"
                        [ List [[Paragraph [Plain "z"]],[Section "Inner" [Paragraph [Plain "x"]]]]
                        ]]),

          ("34. image alt nested inline",
           "<document><header title=\"T34\"></header><body><paragraph><image url=\"i\"><bold>B</bold><italic>I</italic></image></paragraph></body></document>",
           Document (Header "T34" Nothing Nothing) [Paragraph [Image "i" [Bold [Plain "B"],Italic [Plain "I"]]]]),

          ("35. link alt nested inline",
           "<document><header title=\"T35\"></header><body><paragraph><link url=\"u\">go <italic>i</italic></link></paragraph></body></document>",
           Document (Header "T35" Nothing Nothing) [Paragraph [Link "u" [Plain "go ",Italic [Plain "i"]]]]),

          ("36. list item string simple = Raw",
           "<document><header title=\"T36\"></header><body><list>item</list></body></document>",
           Document (Header "T36" Nothing Nothing) [List [[Raw "item"]]]),

          ("37. list item = codeblock",
           "<document><header title=\"T37\"></header><body><list><codeblock>cb</codeblock></list></body></document>",
           Document (Header "T37" Nothing Nothing) [List [[CodeBlock [Raw "cb"]]]]),

          ("38. codeblock deux lignes string = Raw concat",
           "<document><header title=\"T38\"></header><body><codeblock>l1l2</codeblock></body></document>",
           Document (Header "T38" Nothing Nothing) [CodeBlock [Raw "l1l2"]]),

          ("39. codeblock = raw + paragraph",
           "<document><header title=\"T39\"></header><body><codeblock>raw<paragraph>p</paragraph></codeblock></body></document>",
           Document (Header "T39" Nothing Nothing) [CodeBlock [Raw "raw",Paragraph [Plain "p"]]]),

          ("40. section vide",
           "<document><header title=\"T40\"></header><body><section title=\"Empty\"></section></body></document>",
           Document (Header "T40" Nothing Nothing) [Section "Empty" []]),

          ("41. paragraph vide",
           "<document><header title=\"T41\"></header><body><paragraph></paragraph></body></document>",
           Document (Header "T41" Nothing Nothing) [Paragraph []]),

          ("42. image alt vide",
           "<document><header title=\"T42\"></header><body><paragraph><image url=\"img.png\"></image></paragraph></body></document>",
           Document (Header "T42" Nothing Nothing) [Paragraph [Image "img.png" []]]),

          ("43. link content vide",
           "<document><header title=\"T43\"></header><body><paragraph><link url=\"u\"></link></paragraph></body></document>",
           Document (Header "T43" Nothing Nothing) [Paragraph [Link "u" []]]),

          ("44. list vide",
           "<document><header title=\"T44\"></header><body><list></list></body></document>",
           Document (Header "T44" Nothing Nothing) [List []]),

          ("45. raw string vide",
           "<document><header title=\"T45\"></header><body></body></document>",
           Document (Header "T45" Nothing Nothing) []),

          ("46. inline string vide",
           "<document><header title=\"T46\"></header><body><paragraph></paragraph></body></document>",
           Document (Header "T46" Nothing Nothing) [Paragraph []]),

          ("47. list avec Raw vide",
           "<document><header title=\"T47\"></header><body><list></list></body></document>",
           Document (Header "T47" Nothing Nothing) [List []]),

          ("48. deeply nested inline",
           "<document><header title=\"T48\"></header><body><paragraph><italic><bold><code>deep</code></bold></italic></paragraph></body></document>",
           Document (Header "T48" Nothing Nothing) [Paragraph [Italic [Bold [CodeSpan "deep"]]]]),

          ("49. section avec raw + codeblock",
           "<document><header title=\"T49\"></header><body><section title=\"S\">t<codeblock>c</codeblock></section></body></document>",
           Document (Header "T49" Nothing Nothing) [Section "S" [Raw "t",CodeBlock [Raw "c"]]]),

          ("50. paragraph tous inlines",
           "<document><header title=\"T50\"></header><body><paragraph>t<bold>b</bold><italic>i</italic><code>c</code><link url=\"u\">l</link><image url=\"img\">a</image></paragraph></body></document>",
           Document (Header "T50" Nothing Nothing) [Paragraph [Plain "t",Bold [Plain "b"],Italic [Plain "i"],CodeSpan "c",Link "u" [Plain "l"],Image "img" [Plain "a"]]])
        ]

  forM_ staticCases $ \(name, input, expected) ->
    it name $ runP input `shouldBe` Just (expected, "")
