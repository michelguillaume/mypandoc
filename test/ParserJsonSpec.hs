{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ParserJsonSpec (spec) where

import Test.Hspec
import Control.Monad      (forM_)
import Parser.Core        (runParser)
import Parser.JSON        (parseJSONDocument)
import AST                (Document(..), Header(..), Block(..), Inline(..))

runP :: String -> Maybe (Document, String)
runP = runParser parseJSONDocument

spec :: Spec
spec = describe "Parser.JSON.parseJSONDocument (input → expected Document)" $ do

  let staticCases :: [(String, [String], Document)]
      staticCases =
        [ ("01. raw seul"
          , [ "{"
            , "  \"header\": { \"title\": \"T1\" },"
            , "  \"body\": ["
            , "    \"foo\""
            , "  ]"
            , "}"
            ]
          , Document (Header "T1" Nothing Nothing)
                     [Raw "foo"]
          )

        , ("02. paragraphe 1 mot"
          , [ "{"
            , "  \"header\": { \"title\": \"T2\" },"
            , "  \"body\": ["
            , "    [\"bar\"]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T2" Nothing Nothing)
                     [Paragraph [Plain "bar"]]
          )

        , ("03. paragraphe 2 mots"
          , [ "{"
            , "  \"header\": { \"title\": \"T3\" },"
            , "  \"body\": ["
            , "    [\"a\",\"b\"]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T3" Nothing Nothing)
                     [Paragraph [Plain "a", Plain "b"]]
          )

        , ("04. paragraphe 4 mots"
          , [ "{"
            , "  \"header\": { \"title\": \"T4\" },"
            , "  \"body\": ["
            , "    [\"w1\",\"w2\",\"w3\",\"w4\"]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T4" Nothing Nothing)
                     [Paragraph [Plain "w1",Plain "w2",Plain "w3",Plain "w4"]]
          )

        , ("05. bold seul"
          , [ "{"
            , "  \"header\": { \"title\": \"T5\" },"
            , "  \"body\": ["
            , "    [{\"bold\":\"B\"}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T5" Nothing Nothing)
                     [Paragraph [Bold [Plain "B"]]]
          )

        , ("06. italic seul"
          , [ "{"
            , "  \"header\": { \"title\": \"T6\" },"
            , "  \"body\": ["
            , "    [{\"italic\":\"I\"}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T6" Nothing Nothing)
                     [Paragraph [Italic [Plain "I"]]]
          )

        , ("07. codeSpan inline"
          , [ "{"
            , "  \"header\": { \"title\": \"T7\" },"
            , "  \"body\": ["
            , "    [{\"code\":\"C\"}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T7" Nothing Nothing)
                     [Paragraph [CodeSpan "C"]]
          )

        , ("08. link simple"
          , [ "{"
            , "  \"header\": { \"title\": \"T8\" },"
            , "  \"body\": ["
            , "    [{"
            , "      \"link\": {"
            , "        \"url\": \"u\","
            , "        \"content\": [\"c\"]"
            , "      }"
            , "    }]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T8" Nothing Nothing)
                     [Paragraph [Link "u" [Plain "c"]]]
          )

        , ("09. image simple"
          , [ "{"
            , "  \"header\": { \"title\": \"T9\" },"
            , "  \"body\": ["
            , "    [{"
            , "      \"image\": {"
            , "        \"url\": \"img.png\","
            , "        \"alt\": [\"alt\"]"
            , "      }"
            , "    }]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T9" Nothing Nothing)
                     [Paragraph [Image "img.png" [Plain "alt"]]]
          )

        , ("10. link + bold + texte"
          , [ "{"
            , "  \"header\": { \"title\": \"T10\" },"
            , "  \"body\": ["
            , "    [{\"bold\":\"B\"}, \" & \","
            , "     {\"link\": {"
            , "       \"url\": \"u\","
            , "       \"content\": [\"here\"]"
            , "     }}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T10" Nothing Nothing)
                     [Paragraph [Bold [Plain "B"], Plain " & ", Link "u" [Plain "here"]]]
          )

        , ("11. codeblock raw"
          , [ "{"
            , "  \"header\": { \"title\": \"T11\" },"
            , "  \"body\": ["
            , "    {\"codeblock\": \"hi\"}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T11" Nothing Nothing)
                     [CodeBlock [Raw "hi"]]
          )

        , ("12. codeblock tableau"
          , [ "{"
            , "  \"header\": { \"title\": \"T12\" },"
            , "  \"body\": ["
            , "    {\"codeblock\": [\"l1\", \"l2\"]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T12" Nothing Nothing)
                     [CodeBlock [Raw "l1", Raw "l2"]]
          )

        , ("13. liste 1 item"
          , [ "{"
            , "  \"header\": { \"title\": \"T13\" },"
            , "  \"body\": ["
            , "    {\"list\": [[\"i\"]]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T13" Nothing Nothing)
                     [List [[Paragraph [Plain "i"]]]]
          )

        , ("14. liste 2 items"
          , [ "{"
            , "  \"header\": { \"title\": \"T14\" },"
            , "  \"body\": ["
            , "    {\"list\": [[\"a\"], [\"b\"]]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T14" Nothing Nothing)
                     [List [[Paragraph [Plain "a"]],[Paragraph [Plain "b"]]]]
          )

        , ("15. liste avec codeblock"
          , [ "{"
            , "  \"header\": { \"title\": \"T15\" },"
            , "  \"body\": ["
            , "    {\"list\": [[\"x\"], {\"codeblock\": \"c\"}]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T15" Nothing Nothing)
                     [List [[Paragraph [Plain "x"]],[CodeBlock [Raw "c"]]]]
          )

        , ("16. liste avec section"
          , [ "{"
            , "  \"header\": { \"title\": \"T16\" },"
            , "  \"body\": ["
            , "    {\"list\": [[\"y\"], {\"section\": {"
            , "      \"title\": \"S\","
            , "      \"content\": [[\"z\"]]"
            , "    }}]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T16" Nothing Nothing)
                     [List [[Paragraph [Plain "y"]],[Section "S" [Paragraph [Plain "z"]]]]]
          )

        , ("17. nested list"
          , [ "{"
            , "  \"header\": { \"title\": \"T17\" },"
            , "  \"body\": ["
            , "    {\"list\": ["
            , "      [\"i1\",\"i2\"],"
            , "      [{\"list\": [[\"j\"]]}]"
            , "    ]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T17" Nothing Nothing)
                     [List [[Paragraph [Plain "i1",Plain "i2"]],[List [[Paragraph [Plain "j"]]]]]]
          )

        , ("18. section simple"
          , [ "{"
            , "  \"header\": { \"title\": \"T18\" },"
            , "  \"body\": ["
            , "    {\"section\": {"
            , "      \"title\": \"Sec\","
            , "      \"content\": [[\"p\"]]"
            , "    }}]"
            , "}"
            ]
          , Document (Header "T18" Nothing Nothing) [Section "Sec" [Paragraph [Plain "p"]]]
          )

        , ("19. section avec codeblock"
          , [ "{"
            , "  \"header\": { \"title\": \"T19\" },"
            , "  \"body\": ["
            , "    {\"section\": {"
            , "      \"title\": \"S2\","
            , "      \"content\": [{\"codeblock\": \"c2\"}]"
            , "    }}]"
            , "}"
            ]
          , Document (Header "T19" Nothing Nothing) [Section "S2" [CodeBlock [Raw "c2"]]]
          )

        , ("20. section avec liste"
          , [ "{"
            , "  \"header\": { \"title\": \"T20\" },"
            , "  \"body\": ["
            , "    {\"section\": {"
            , "      \"title\": \"S3\","
            , "      \"content\": ["
            , "        {\"list\": [[\"k\"]]}"
            , "      ]"
            , "    }}]"
            , "}"
            ]
          , Document (Header "T20" Nothing Nothing) [Section "S3" [List [[Paragraph [Plain "k"]]]]]
          )

        , ("21. section imbriquée & mixte"
          , [ "{"
            , "  \"header\": { \"title\": \"T21\" },"
            , "  \"body\": ["
            , "    {\"section\": {"
            , "      \"title\": \"Root\","
            , "      \"content\": ["
            , "        [\"a\"],"
            , "        {\"list\": [[\"l1\"], [\"l2\"]]},"
            , "        {\"section\": {"
            , "          \"title\": \"Child\","
            , "          \"content\": [[\"c\"]]"
            , "        }}"
            , "      ]"
            , "    }}]"
            , "}"
            ]
          , Document (Header "T21" Nothing Nothing)
                     [Section "Root"
                       [ Paragraph [Plain "a"]
                       , List [[Paragraph [Plain "l1"]],[Paragraph [Plain "l2"]]]
                       , Section "Child" [Paragraph [Plain "c"]]
                       ]
                     ]
          )

        , ("22. doc mixte plusieurs blocks"
          , [ "{"
            , "  \"header\": { \"title\": \"T22\" },"
            , "  \"body\": ["
            , "    \"raw\","
            , "    [\"p1\",\"p2\"],"
            , "    {\"codeblock\": \"cb\"},"
            , "    {\"list\": [[\"i\"]]},"
            , "    {\"section\": {"
            , "      \"title\": \"S4\","
            , "      \"content\": [[\"z\"]]"
            , "    }}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T22" Nothing Nothing)
                     [ Raw "raw"
                     , Paragraph [Plain "p1",Plain "p2"]
                     , CodeBlock [Raw "cb"]
                     , List [[Paragraph [Plain "i"]]]
                     , Section "S4" [Paragraph [Plain "z"]]
                     ]
          )

        , ("23. empty body"
          , [ "{"
            , "  \"header\": { \"title\": \"TE\" },"
            , "  \"body\": []"
            , "}"
            ]
          , Document (Header "TE" Nothing Nothing) []
          )

        , ("24. header author+date"
          , [ "{"
            , "  \"header\": { \"title\": \"H\", \"author\": \"Au\", \"date\": \"D\" },"
            , "  \"body\": []"
            , "}"
            ]
          , Document (Header "H" (Just "Au") (Just "D")) []
          )

        , ("25. header date only"
          , [ "{"
            , "  \"header\": { \"title\": \"HD\", \"date\": \"2025-01-01\" },"
            , "  \"body\": []"
            , "}"
            ]
          , Document (Header "HD" Nothing (Just "2025-01-01")) []
          )

        , ("26. nested bold in italic"
          , [ "{"
            , "  \"header\": { \"title\": \"T26\" },"
            , "  \"body\": ["
            , "    [{\"italic\":[{\"bold\":\"B\"}]}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T26" Nothing Nothing) [Paragraph [Italic [Bold [Plain "B"]]]]
          )

        , ("27. nested italic in bold"
          , [ "{"
            , "  \"header\": { \"title\": \"T27\" },"
            , "  \"body\": ["
            , "    [{\"bold\":[{\"italic\":\"I\"}]}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T27" Nothing Nothing) [Paragraph [Bold [Italic [Plain "I"]]]]
          )

        , ("28. mixed inline all types"
          , [ "{"
            , "  \"header\": { \"title\": \"T28\" },"
            , "  \"body\": ["
            , "    [\"start\",{\"bold\":\"B\"},{\"italic\":\"I\"},{\"code\":\"C\"},"
            , "     {\"link\":{\"url\":\"u\",\"content\":[\"L\"]}},"
            , "     {\"image\":{\"url\":\"i\",\"alt\":[\"X\"]}},\"end\"]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T28" Nothing Nothing)
                     [Paragraph
                       [ Plain "start"
                       , Bold [Plain "B"]
                       , Italic [Plain "I"]
                       , CodeSpan "C"
                       , Link "u" [Plain "L"]
                       , Image "i" [Plain "X"]
                       , Plain "end"
                       ]
                     ]
          )

        , ("29. list item with multiple inlines"
          , [ "{"
            , "  \"header\": { \"title\": \"T29\" },"
            , "  \"body\": ["
            , "    {\"list\": [[\"a\",{\"code\":\"c\"},\"b\"]]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T29" Nothing Nothing)
                     [List [[Paragraph [Plain "a",CodeSpan "c",Plain "b"]]]]
          )

        , ("30. deeply nested lists"
          , [ "{"
            , "  \"header\": { \"title\": \"T30\" },"
            , "  \"body\": ["
            , "    {\"list\":["
            , "      [{\"list\": [[\"x\"]]}],"
            , "      [{\"list\":[[\"y\"]]}]"
            , "    ]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T30" Nothing Nothing)
                     [ List
                         [ [ List [[Paragraph [Plain "x"]]] ]
                         , [ List [[Paragraph [Plain "y"]]] ]
                         ]
                     ]
          )

        , ("31. nested codeblock object"
          , [ "{"
            , "  \"header\": { \"title\": \"T31\" },"
            , "  \"body\": ["
            , "    {\"codeblock\":{\"codeblock\":\"inner\"}}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T31" Nothing Nothing) [CodeBlock [CodeBlock [Raw "inner"]]]
          )

                , ("32. section with mixed content"
          , [ "{"
            , "  \"header\": { \"title\": \"T32\" },"
            , "  \"body\": ["
            , "    {\"section\":{"
            , "       \"title\":\"Mix\","
            , "       \"content\":["
            , "         \"t\","
            , "         [{\"bold\":\"B\"}],"
            , "         {\"codeblock\":\"C\"},"
            , "         {\"list\":[[\"L\"]]}"
            , "       ]}"
            , "    }]"
            , "}"
            ]
          , Document (Header "T32" Nothing Nothing)
                     [ Section "Mix"
                         [ Raw "t"
                         , Paragraph [Bold [Plain "B"]]
                         , CodeBlock [Raw "C"]
                         , List [[Paragraph [Plain "L"]]]
                         ]
                     ]
          )

                , ("33. section in list in section"
          , [ "{"
            , "  \"header\": { \"title\": \"T33\" },"
            , "  \"body\": ["
            , "    {\"section\":{"
            , "       \"title\":\"Top\","
            , "       \"content\":["
            , "         {\"list\":["
            , "           [\"z\"],"
            , "           {\"section\":{\"title\":\"Inner\",\"content\":[[\"x\"]]}}"
            , "         ]}"
            , "       ]}"
            , "    }]"
            , "}"
            ]
          , Document (Header "T33" Nothing Nothing)
                     [ Section "Top"
                         [ List [[Paragraph [Plain "z"]]
                                ,[Section "Inner" [Paragraph [Plain "x"]]]
                         ]
                     ]
                     ]
          )

        , ("34. image alt nested inline"
          , [ "{"
            , "  \"header\": { \"title\": \"T34\" },"
            , "  \"body\": ["
            , "    [{\"image\":{\"url\":\"i\",\"alt\":[{\"bold\":\"B\"},{\"italic\":\"I\"}]}}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T34" Nothing Nothing)
                     [Paragraph [Image "i" [Bold [Plain "B"],Italic [Plain "I"]]]]
          )

        , ("35. link alt nested inline"
          , [ "{"
            , "  \"header\": { \"title\": \"T35\" },"
            , "  \"body\": ["
            , "    [{\"link\":{\"url\":\"u\",\"content\":[\"go \",{\"italic\":\"i\"}]}}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T35" Nothing Nothing)
                     [Paragraph [Link "u" [Plain "go ",Italic [Plain "i"]]]]
          )
        

        , ( "36. list item string simple = Raw"
          , [ "{"
            , "  \"header\": { \"title\": \"T36\" },"
            , "  \"body\": ["
            , "    {\"list\": [\"item\"]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T36" Nothing Nothing) [List [[Raw "item"]]]
          )

        , ( "37. list item = codeblock"
          , [ "{"
            , "  \"header\": { \"title\": \"T37\" },"
            , "  \"body\": ["
            , "    {\"list\": ["
            , "      {\"codeblock\": \"cb\"}"
            , "    ]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T37" Nothing Nothing) [List [[CodeBlock [Raw "cb"]]]]
          )

        , ( "38. codeblock deux lignes string = Raw concat"
          , [ "{"
            , "  \"header\": { \"title\": \"T38\" },"
            , "  \"body\": ["
            , "    {\"codeblock\": [\"l1\", \"l2\"]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T38" Nothing Nothing) [CodeBlock [Raw "l1", Raw "l2"]]
          )

        , ( "39. codeblock = raw + paragraph"
          , [ "{"
            , "  \"header\": { \"title\": \"T39\" },"
            , "  \"body\": ["
            , "    {\"codeblock\": ["
            , "      \"raw\"," 
            , "      [\"p\"]"
            , "    ]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T39" Nothing Nothing) [CodeBlock [Raw "raw", Paragraph [Plain "p"]]]
          )

        , ( "40. section vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T40\" },"
            , "  \"body\": ["
            , "    {\"section\": {\"title\": \"Empty\", \"content\": []}}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T40" Nothing Nothing) [Section "Empty" []]
          )

        , ( "41. paragraph vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T41\" },"
            , "  \"body\": ["
            , "    []"
            , "  ]"
            , "}"
            ]
          , Document (Header "T41" Nothing Nothing) [Paragraph []]
          )

        , ( "42. image alt vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T42\" },"
            , "  \"body\": ["
            , "    [{\"image\": {\"url\": \"img.png\", \"alt\": []}}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T42" Nothing Nothing) [Paragraph [Image "img.png" []]]
          )

        , ( "43. link content vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T43\" },"
            , "  \"body\": ["
            , "    [{\"link\": {\"url\": \"u\", \"content\": []}}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T43" Nothing Nothing) [Paragraph [Link "u" []]]
          )

        , ( "44. list vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T44\" },"
            , "  \"body\": ["
            , "    {\"list\": []}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T44" Nothing Nothing) [List []]
          )

        , ( "45. raw string vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T45\" },"
            , "  \"body\": [\"\"]"
            , "}"
            ]
          , Document (Header "T45" Nothing Nothing) [Raw ""]
          )

        , ( "46. inline string vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T46\" },"
            , "  \"body\": [[\"\"]]"
            , "}"
            ]
          , Document (Header "T46" Nothing Nothing) [Paragraph [Plain ""]]
          )

        , ( "47. list avec Raw vide"
          , [ "{"
            , "  \"header\": { \"title\": \"T47\" },"
            , "  \"body\": ["
            , "    {\"list\": [\"\"]}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T47" Nothing Nothing) [List [[Raw ""]]]
          )

        , ( "48. deeply nested inline"
          , [ "{"
            , "  \"header\": { \"title\": \"T48\" },"
            , "  \"body\": ["
            , "    [{\"italic\": [{\"bold\": [{\"code\": \"deep\"}]}]}]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T48" Nothing Nothing) [Paragraph [Italic [Bold [CodeSpan "deep"]]]]
          )

        , ( "49. section avec raw + codeblock"
          , [ "{"
            , "  \"header\": { \"title\": \"T49\" },"
            , "  \"body\": ["
            , "    {\"section\": {"
            , "      \"title\": \"S\","
            , "      \"content\": [\"t\", {\"codeblock\": \"c\"}]"
            , "    }}"
            , "  ]"
            , "}"
            ]
          , Document (Header "T49" Nothing Nothing) [Section "S" [Raw "t", CodeBlock [Raw "c"]]]
          )

        , ( "50. paragraph tous inlines"
          , [ "{"
            , "  \"header\": { \"title\": \"T50\" },"
            , "  \"body\": ["
            , "    ["
            , "      \"t\","
            , "      {\"bold\": \"b\"},"
            , "      {\"italic\": \"i\"},"
            , "      {\"code\": \"c\"},"
            , "      {\"link\": {\"url\": \"u\", \"content\": [\"l\"]}},"
            , "      {\"image\": {\"url\": \"img\", \"alt\": [\"a\"]}}"
            , "    ]"
            , "  ]"
            , "}"
            ]
          , Document (Header "T50" Nothing Nothing)
              [Paragraph
                [ Plain "t"
                , Bold [Plain "b"]
                , Italic [Plain "i"]
                , CodeSpan "c"
                , Link "u" [Plain "l"]
                , Image "img" [Plain "a"]
                ]]
          )
        ]


  forM_ staticCases $ \(name, jsLines, expected) ->
    it name $
      runP (unlines jsLines) `shouldBe` Just (expected, "")
