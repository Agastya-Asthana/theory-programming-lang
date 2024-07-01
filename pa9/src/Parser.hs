{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
  )
where

import qualified Data.Text.Lazy as L
import Lexer
import Syntax
import Text.Parsec
  ( ParseError,
    choice,
    many,
    many1,
    optional,
    parse,
    sepBy,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Char (char, spaces)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Text.Lazy (Parser)
import qualified Text.Parsec.Token as Tok

patternParser :: Parser Pattern
patternParser =
  try pcons
    <|> try plit
    <|> try pvar

pvar :: Parser Pattern
pvar = PVar <$> identifier

pcons :: Parser Pattern
pcons = do
  _ <- char '('
  x <- identifier
  _ <- char ':'
  xs <- identifier
  _ <- char ')'
  _ <- spaces
  return $ PCons x xs

plit :: Parser Pattern
plit = PLit <$> choice [try parseInt, try parseBool, try parseList]

integer :: Parser Integer
integer = Tok.natural lexer

variable :: Parser Expr
variable = do
  Var <$> identifier

number :: (Lit -> a) -> Parser a
number c = do
  c . LInt . fromIntegral <$> integer

bool :: (Lit -> a) -> Parser a
bool c =
  (reserved "True" >> return (c (LBool True)))
    <|> (reserved "False" >> return (c (LBool False)))

-- Parse an integer literal
parseInt :: Parser Lit
parseInt = LInt <$> Tok.integer lexer

-- Parse a list
parseList :: Parser Lit
parseList = LArray <$> Tok.brackets lexer (parseLit `sepBy` Tok.comma lexer)

litToExpr :: Lit -> Expr
litToExpr (LInt n) = Lit (LInt n)
litToExpr (LBool b) = Lit (LBool b)
litToExpr (LArray lits) = Lit (LArray lits)

litsToExprs :: [Lit] -> [Expr]
litsToExprs = map litToExpr

parseBool :: Parser Lit
parseBool =
  (reserved "True" >> return (LBool True))
    <|> (reserved "False" >> return (LBool False))

-- Parse a single literal
parseLit :: Parser Expr
parseLit = litToExpr <$> choice [try parseInt, try parseBool, try parseList]

-- Parse a list of literals
list :: (Lit -> a) -> Parser a
list c = do
  lits <- Tok.brackets lexer (parseLit `sepBy` Tok.comma lexer)
  return (c (LArray lits))

fix :: Parser Expr
fix = do
  reservedOp "fix"
  Fix <$> expr

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many patternParser
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

aexp :: Parser Expr
aexp =
  parens expr
    <|> list Lit
    <|> bool Lit
    <|> number Lit
    <|> ifthen
    <|> fix
    <|> try letrecin
    <|> letin
    <|> lambda
    <|> variable

term :: Parser Expr
term =
  aexp >>= \x ->
    (many1 aexp >>= \xs -> return (foldl App x xs))
      <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table =
  [ [ infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [ infixOp "+" (Op Add) Ex.AssocLeft,
      infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [ infixOp "==" (Op Eql) Ex.AssocLeft
    ],
    [ infixOp ":" (Op Cons) Ex.AssocRight
    ],
    [ infixOp "++" (Op Concat) Ex.AssocNone
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many patternParser
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  PVar name <- patternParser
  args <- many patternParser
  reservedOp "="
  body <- expr
  let namePattern = PVar name
  return (name, Fix $ foldr Lam body (namePattern : args))

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl :: Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule :: FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)
