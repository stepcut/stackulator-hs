{-# language OverloadedStrings #-}
module Parser where

import Data.Text (Text)
import qualified Data.Text as Text
import Stackulator
import Text.Parsec
{-
Syntax is hard, let's go shopping!

This version of the syntax is easy to parse -- no backtracking required?

let VAR = TERM
VAR
LIT
\VAR[:TERM]. TERM
(TERM TERM)

-}
{-

let VAR = TERM
-- pTerm
VAR
LIT
\VAR0 [.. VAR_N] => TERM
TERM0 [.. TERM_N]
(TERM)


Syntax examples:

let id    = \ a x => x : forall (a : *) . a -> a
let const = (\ a b x y => x) : forall (a : *) (b : *) . a -> b -> a

3
+

3 : Double

How are parens handled?

-}

type Parser = Parsec String ()

specials :: [Char]
specials = [' ', '.', ':', '(', ')', '\\']

reserved :: [Text]
reserved = ["let", "forall", "=>", "Type"]


-- | parse a `Var`
pVar :: Parser Var
pVar =
  do v <- Text.pack <$> (many1 $ satisfy (\c -> not (c `elem` specials)))
     if (v `elem` reserved)
      then parserFail $ Text.unpack $ v <> " is a reserved keyword"
      else pure (Name v)

-- | parse a `Var` and lift it to a `Term`
pTVar :: Parser Term
pTVar = TVar <$> pVar


-- | FIXME
pDouble :: Parser Literal
pDouble =
  do str <- many1 $ satisfy (\c -> c `elem` ("1234567890.-" :: String))
     pure (LDouble $ read $ str)


pNat :: Parser Int
pNat =
  do str <- many1 digit
     pure (read $ str)


-- | parse any `Literal`
pLiteral :: Parser Literal
pLiteral = pDouble

-- | parse a `Literal` and lift it to a `Term`
pLit :: Parser Term
pLit = Lit <$> pLiteral

-- | parse `Primitive`
pPrimitive :: Parser Primitive
pPrimitive =
        ((char '+') >> pure Plus)  <|>
        ((char '-') >> pure Minus) <|>
        ((char '*') >> pure Times) <|>
        ((char '/') >> pure Divide)


-- | parse `Primitive` and lift to `Term`
pPrim :: Parser Term
pPrim = Prim <$> pPrimitive


commitTo = id

-- | parse a `Universe` `Term`
pUniverse :: Parser Term
pUniverse =
  try (string "Type") >>
     (commitTo $ ((do space
                      i <- pNat
                      return (Universe i)
                 ) <|>
                 (do return (Universe 0))
                 ))

colon = char ':'
dot = char '.'

-- | parse a `Pi` `Term`
pPi :: Parser Term
pPi =
  try (string "forall ") >>
       do v <- pVar
          many space
          colon
          ty <- pTerm
          dot
          many space
          e <- pTerm
          return (Pi v ty e)



-- | parse a `Lambda` `Term`
pLambda :: Parser Term
pLambda =
  do string "\\"
     vtys <- commitTo $ many1 $ do many space
                                   v <- pVar <* many space
                                   colon
                                   ty <- commitTo $ pTerm
                                   pure (v, ty)
     many space
--     string "=>"
     commitTo $ char '.'
     many space
     e <- commitTo pTerm
     return (foldr (\(v,ty) -> \e' ->  Lambda v ty e') e vtys)

-- | parse application (term0 .. termN)
pApp :: Parser Term
pApp =
     do char '('
        (t:ts) <- reverse <$> sepBy1 pTerm space
        char ')'
        return $ foldl App t ts


pTerm :: Parser Term
pTerm = pPrim <|> pUniverse <|> pPi <|> pLit <|> pTVar <|> pLambda <|> pApp


pLet :: Parser Statement
pLet =
     try (string "let ") >>
       do v <- pVar
          many space
          char '='
          many space
          t <- pTerm
          commitTo $ return $ Let v t


pAssume :: Parser Statement
pAssume =
     try (string "assume ") >>
       do v <- pVar
          many space
          char ':'
          many space
          t <- pTerm
          commitTo $ return $ Assume v t

pForget :: Parser Statement
pForget =
  try (string "forget ") >>
    do v <- pVar
       commitTo $ return $ Forget v

pStatement :: Parser Statement
pStatement =
  pLet <|> pAssume <|> pForget <|> (STerm <$> pTerm)
