{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- JSON parser
-}

module Parser.JSON (parseJSONDocument) where

import Parser.Core       (Parser, lexeme, char, parseQuoted)
import AST               (Document(Document), Header(Header),
                          Block(..), Inline(..))
import Control.Applicative ((<|>), many)

data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String
  deriving (Show)

isInlineVal :: JValue -> Bool
isInlineVal (JString _)             = True
isInlineVal (JObject [("bold",_)])  = True
isInlineVal (JObject [("italic",_)])= True
isInlineVal (JObject [("code",_)])  = True
isInlineVal (JObject [("link",_)])  = True
isInlineVal (JObject [("image",_)]) = True
isInlineVal _                       = False

parseJSONDocument :: Parser Document
parseJSONDocument =
  lexeme parseJValue >>= convertDoc

parseJValue :: Parser JValue
parseJValue = lexeme (parseObject <|> parseArray <|> (JString <$> parseQuoted))

parseObject :: Parser JValue
parseObject = do
  _   <- lexeme (char '{')
  kvs <- sepBy parsePair (lexeme (char ','))
  _   <- lexeme (char '}')
  return (JObject kvs)

parsePair :: Parser (String, JValue)
parsePair = do
  key <- parseQuoted
  _   <- lexeme (char ':')
  val <- parseJValue
  return (key, val)

parseArray :: Parser JValue
parseArray = do
  _   <- lexeme (char '[')
  vs  <- sepBy parseJValue (lexeme (char ','))
  _   <- lexeme (char ']')
  return (JArray vs)

convertDoc :: JValue -> Parser Document
convertDoc (JObject kv) = Document <$> parseHeader kv <*> parseBody kv
convertDoc other       = fail $ "Expected top-level obj, got: " ++ show other

-- Helpers to parse header and body separately
parseHeader :: [(String,JValue)] -> Parser Header
parseHeader kv = do
  hdrVal <- lookupVal "header" kv >>= unwrapObject "\"header\" must be an obj"
  title  <- lookupStr "title" hdrVal
  auth   <- lookupOpt "author" hdrVal
  date   <- lookupOpt "date" hdrVal
  return $ Header title auth date

parseBody :: [(String,JValue)] -> Parser [Block]
parseBody kv = do
  bodyVal <- lookupVal "body" kv >>= unwrapArray "\"body\" must be an array"
  mapM convertBlock bodyVal

-- Unwrapping utilities
unwrapObject :: String -> JValue -> Parser [(String,JValue)]
unwrapObject msg (JObject o) = return o
unwrapObject msg _           = fail msg

unwrapArray :: String -> JValue -> Parser [JValue]
unwrapArray msg (JArray a) = return a
unwrapArray msg _          = fail msg

convertBlock :: JValue -> Parser Block
convertBlock jv = case jv of
  JString s -> return (Raw s)
  JArray xs -> Paragraph <$> mapM convertInline xs
  JObject [(key,val)] -> parseByKey key val
  JObject kvs -> fail $ "Expected single-key object for Block, got keys: "
    ++ show (map fst kvs)
  other -> fail $ "Unexpected JSON value for Block: " ++ show other

parseByKey :: String -> JValue -> Parser Block
parseByKey key val = case key of
  "section"   -> parseSection val
  "codeblock" -> parseCodeBlock val
  "list"      -> parseList val
  _           -> fail $ "Unknown block type: " ++ show key

parseSection :: JValue -> Parser Block
parseSection (JObject kv) = do
  title <- lookupStr "title" kv
  cVal  <- lookupVal "content" kv
  xs    <- case cVal of
             JArray bs -> return bs
             _         -> fail "\"content\" of section must be an array"
  subs <- mapM convertBlock xs
  return (Section title subs)

parseSection other = fail $ "Expected object for section, got: " ++ show other

parseCodeBlock :: JValue -> Parser Block
parseCodeBlock jv = case jv of
  JString s -> return $ CodeBlock [Raw s]

  JArray vs -> CodeBlock <$> mapM convertBlock vs

  JObject _ -> (\b -> CodeBlock [b]) <$> convertBlock jv

  _ -> fail $ "Unexpected JSON for codeblock: " ++ show jv

parseList :: JValue -> Parser Block
parseList (JArray items) = do
  xss <- mapM parseListItem items
  return (List xss)
parseList other =
  fail $ "Expected array for list, got: " ++ show other

parseListItem :: JValue -> Parser [Block]
parseListItem jv = case jv of
  JString s -> return [Raw s]
  JArray vs | all isInlineVal vs -> do
    inls <- mapM convertInline vs
    return [Paragraph inls]
  JArray vs -> mapM convertBlock vs
  JObject _ -> (\b -> [b]) <$> convertBlock jv
  other -> fail $ "Unexpected JSON in list item: " ++ show other

-- Inline parsing split into specialized functions
convertInline :: JValue -> Parser Inline
convertInline jv = case jv of
  JString s                  -> parsePlain s
  JObject [("bold",   v)]    -> parseBold v
  JObject [("italic", v)]    -> parseItalic v
  JObject [("code",   JString s)] -> return (CodeSpan s)
  JObject [("link",   lp)]    -> parseLink lp
  JObject [("image",  ip)]    -> parseImage ip
  _ -> fail $ "Unknown inline element: " ++ show jv

parsePlain :: String -> Parser Inline
parsePlain = return . Plain

parseBold :: JValue -> Parser Inline
parseBold (JString s) = return $ Bold [Plain s]
parseBold (JArray xs) = Bold <$> mapM convertInline xs
parseBold _           = fail "Invalid bold content"

parseItalic :: JValue -> Parser Inline
parseItalic (JString s) = return $ Italic [Plain s]
parseItalic (JArray xs) = Italic <$> mapM convertInline xs
parseItalic _           = fail "Invalid italic content"

parseLink :: JValue -> Parser Inline
parseLink (JObject lp) = do
  url  <- lookupStr "url" lp
  cVal <- lookupVal "content" lp
  inls <- case cVal of
             JArray ys -> return ys
             _         -> fail "\"content\" of link must be an array"
  Link url <$> mapM convertInline inls
parseLink _ = fail "Invalid link object"

parseImage :: JValue -> Parser Inline
parseImage (JObject ip) = do
  url  <- lookupStr "url" ip
  aVal <- lookupVal "alt" ip
  alts <- case aVal of
             JArray xs -> return xs
             _         -> fail "\"alt\" of image must be an array"
  Image url <$> mapM convertInline alts
parseImage _ = fail "Invalid image object"

lookupVal :: String -> [(String,JValue)] -> Parser JValue
lookupVal k kvs = case lookup k kvs of
  Just v  -> return v
  Nothing -> fail $ "Missing key \"" ++ k ++ "\"; available: "
                    ++ show (map fst kvs)

lookupStr :: String -> [(String,JValue)] -> Parser String
lookupStr k kvs = case lookup k kvs of
  Just (JString s) -> return s
  Just _           -> fail $ "Expected string at key \""
    ++ k ++ "\"; got non-string"
  Nothing          -> fail $ "Missing key \"" ++ k ++ "\"; available: "
                             ++ show (map fst kvs)

lookupOpt :: String -> [(String,JValue)] -> Parser (Maybe String)
lookupOpt k kvs = case lookup k kvs of
  Just (JString s) -> return (Just s)
  Just _           -> fail $ "Expected string for optional key \"" ++ k ++ "\""
  Nothing          -> return Nothing

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []
