module Parser where

import           Common
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           Data.Set
import qualified Data.List                     as L

-----------------------
-- Función para facilitar el testing del parser
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace reAl
  t <- p
  eof
  return t

-- Analizador de Tokens
reAl :: TokenParser u
reAl = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , reservedNames   = ["def", "sel", "proj", "rename", "union", "diff", "prod"]
    , reservedOpNames = [".", "=", "-", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!"]
    }
  )

-----------------------------------
--- Parser de valores
-----------------------------------

parseString :: Parser Value
parseString = do s <- stringLiteral reAl
                 return (StringVal s)

parseInt :: Parser Value
parseInt = do i <- integer reAl
              return (IntVal (fromIntegral i))

parseDouble :: Parser Value
parseDouble = try (do reservedOp reAl "-"
                      d <- float reAl
                      return (DoubleVal (-d)))
              <|> do d <- float reAl
                     return (DoubleVal d)

parseNum :: Parser Value
parseNum = try (do i <- integer reAl
                   symbol reAl ".0"
                   return (IntVal (fromIntegral i)))
           <|> try parseDouble
           <|> parseInt

parseAttribute :: Parser Value
parseAttribute = do n <- identifier reAl
                    try (do reservedOp reAl "."
                            a <- identifier reAl
                            return (AttrVal n a))
                        <|> return (AttrVal "" n)

parseValue :: Parser Value
parseValue = try parseString
             <|> try parseNum
             <|> parseAttribute

-----------------------------------
--- Parser de condiciones
-----------------------------------

parseEq :: Parser (Value -> Value -> Cond)
parseEq = do reservedOp reAl "=="
             return Eq

parseNEq :: Parser (Value -> Value -> Cond)
parseNEq = do reservedOp reAl "!="
              return NEq

parseLt :: Parser (Value -> Value -> Cond)
parseLt = do reservedOp reAl "<"
             return Lt

parseGt :: Parser (Value -> Value -> Cond)
parseGt = do reservedOp reAl ">"
             return Gt

parseLtE :: Parser (Value -> Value -> Cond)
parseLtE = do reservedOp reAl "<="
              return LtE

parseGtE :: Parser (Value -> Value -> Cond)
parseGtE = do reservedOp reAl ">="
              return GtE

parseAnd :: Parser (Cond -> Cond -> Cond)
parseAnd = do reservedOp reAl "&&"
              return And

parseOr :: Parser (Cond -> Cond -> Cond)
parseOr = do reservedOp reAl "||"
             return Or

parseNot :: Parser Cond
parseNot = do reservedOp reAl "!"
              c <- parens reAl parseCond
              return (Not c)

parseComparison :: Parser Cond
parseComparison = do v1 <- parseValue
                     op <- try parseEq
                           <|> try parseNEq
                           <|> try parseLt
                           <|> try parseGt
                           <|> try parseLtE
                           <|> parseGtE
                     v2 <- parseValue
                     return (op v1 v2)

parseCond' :: Parser Cond
parseCond' = try parseNot
             <|> try parseComparison
             <|> parens reAl parseCond

parseCond :: Parser Cond
parseCond = try (chainl1 (chainl1 (parseCond') parseAnd) parseOr)

-----------------------------------
--- Parser de operaciones
-----------------------------------

parseSel :: Parser (Relation -> Relation)
parseSel = do reserved reAl "sel"
              c <- parens reAl parseCond
              return (Sel c)

parseProj :: Parser (Relation -> Relation)
parseProj = do reserved reAl "proj"
               as <- parens reAl (commaSep1 reAl parseAttribute)
               return (Proj as)

parseRename :: Parser (Relation -> Relation)
parseRename = do reserved reAl "rename"
                 n <- parens reAl (identifier reAl)
                 return (Rename n)

parseUnion :: Parser (Relation -> Relation -> Relation)
parseUnion = do reserved reAl "union"
                return Union

parseDiff :: Parser (Relation -> Relation -> Relation)
parseDiff = do reserved reAl "diff"
               return Diff

parseProd :: Parser (Relation -> Relation -> Relation)
parseProd = do reserved reAl "prod"
               return Prod

parseUnaryOp :: Parser Relation
parseUnaryOp = do op <- try parseSel
                        <|> try parseProj
                        <|> parseRename
                  rel <- parens reAl parseRelation
                  return (op rel)

parseBinaryOp :: Parser Relation
parseBinaryOp = do op <- try parseUnion
                         <|> try parseDiff
                         <|> parseProd
                   (rel1,rel2) <- parens reAl (do rel1 <- parseRelation; comma reAl
                                                  rel2 <- parseRelation; return (rel1,rel2))
                   return (op rel1 rel2)

parseOperation :: Parser Relation
parseOperation = try parseUnaryOp
                 <|> try parseBinaryOp
                 <|> do n <- identifier reAl
                        args <- parens reAl (commaSep1 reAl (identifier reAl))
                        return (Op n args)

parseRelation :: Parser Relation
parseRelation = try parseOperation
                <|> try (do n <- identifier reAl
                            return (Table n))
                <|> parens reAl parseRelation

-----------------------------------
--- Parser de tablas
-----------------------------------

parseAttrName :: Parser Value
parseAttrName = do a <- identifier reAl
                   return (AttrVal "" a)

parseFirstRow :: Int -> Parser [Value]
parseFirstRow 1 = do v <- try parseString
                          <|> parseNum
                     return [v]
parseFirstRow n = do [v] <- parseFirstRow 1
                     comma reAl
                     vs <- parseFirstRow (n-1)
                     return (v:vs)

parseOtherRow :: [Type] -> Parser [Value]
parseOtherRow [t]    = do a <- case t of
                                 Str -> parseString
                                 _   -> parseNum
                          return [a]
parseOtherRow (t:ts) = do [a] <- parseOtherRow [t]
                          comma reAl
                          as <- parseOtherRow ts
                          return (a:as)

parseRows :: Int -> Parser ([Type], Set Row)
parseRows n = do r1 <- parseFirstRow n
                 let ts = L.map types r1
                 rs <- many (parseOtherRow ts)
                 return (ts, fromList (r1:rs))
              where
                types :: Value -> Type
                types (StringVal _) = Str
                types _             = Num

parseTable :: Parser Table
parseTable = do as <- commaSep1 reAl parseAttrName
                let n = length as
                (ts, rs) <- parseRows n
                return (T as ts rs)

-----------------------------------
--- Parser de comandos
-----------------------------------

parseDefRel :: Parser Stmt
parseDefRel = do n <- identifier reAl
                 reservedOp reAl "="
                 r <- parseRelation
                 return (DefRel n r)

parseDefTable :: Parser Stmt
parseDefTable = do n <- identifier reAl
                   reservedOp reAl "="
                   t <- braces reAl parseTable
                   return (DefTable n t)

parseDefOp :: Parser Stmt
parseDefOp = do reserved reAl "def"
                n <- identifier reAl
                args <- parens reAl (commaSep1 reAl (identifier reAl))
                reservedOp reAl "="
                op <- parseOperation
                return (DefOp n args op)

parseStmt :: Parser Stmt
parseStmt = try parseDefRel
            <|> try parseDefOp
            <|> do r <- parseRelation
                   return (Eval r)

-----------------------------------
--- Parser de archivos
-----------------------------------

parseFile :: Parser [Stmt]
parseFile = many (try parseDefTable <|> try parseDefRel <|> parseDefOp)
