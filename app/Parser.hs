module Parser where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec 
import Text.Megaparsec.Char
import Ast

import qualified Text.Megaparsec.Char.Lexer as L
type Parser = Parsec Void Text



sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

symbol :: Text -> Parser Text
symbol = L.symbol sc
  
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")

parseName :: Parser Name
parseName =  do
  a <- lowerChar
  b <- many letterChar
  return (Name (T.pack $ ([a]<> b)))

-- agent names are slightly different than how they are defined in
-- inpla, all agent names need to start with upper case in this impl
parseAgent :: Parser Agent
parseAgent =
  try parseAgent1
  <|> parseAgent2
  where
    parseAName = do
      a <- upperChar
      b <- many letterChar
      return (T.pack ([a] <> b))
    parseAgent1 = do
      an <- parseAName
      (op:pts) <- parens $ parseTerm `sepBy1` (symbol ",") 
      return (Agent an (Just op) pts)
    parseAgent2 = pure (\i -> Agent { port = [], aid = i, oport = Nothing }) <*> parseAName
    

parseTerm :: Parser Term
parseTerm = lexeme pt
  where
    pt = (pure N <*> parseName)  <|> (pure A <*> parseAgent)

parseConnection :: Parser Connection
parseConnection = do
  a <- parseTerm
  _ <- symbol "~"
  b <- parseTerm
  return $ Conn a b


parseIRule :: Parser InteractionRule
parseIRule = do
  a1 <- lexeme parseAgent
  _ <- symbol "><"
  a2 <- lexeme parseAgent
  _ <- symbol "=>"
  cs <- parseConnection `sepBy1` (symbol ",")
  -- symbol ";" - add semis later
  return (IRule a1 a2 cs)

parseExpr :: Parser Expr
parseExpr = e <* symbol ";"

  where
    e = try (pure ARule <*> parseIRule)
        <|> try (pure ACon <*> parseConnection)
        <|> (pure ATerm <*> parseTerm)
