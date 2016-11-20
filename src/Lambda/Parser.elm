module Lambda.Parser exposing(..)

import Combine as Parser exposing (Parser)
import Combine.Char as Parser
import String
import Lambda.Ast exposing (..)


showTerm : Term a -> String
showTerm term =
  case term of
    Var _ x ->
      x
    Abs _ x b ->
      "\\" ++ x ++ "." ++ showTerm b 
    App _ a b ->
      "@ " ++ showTerm a ++ " " ++ showTerm b


isAlphaNumLower c =
  String.contains (String.fromChar c) "abcdefghijklmnopqrstuvwxyz_$" 


isAlphaNum c =
  String.contains (String.fromChar c) "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_$" 


isDigit c =
  String.contains (String.fromChar c) "0123456789" 


isWhitespace c =
  String.contains (String.fromChar c) " \n\r\t" 


whitespace = Parser.many (Parser.satisfy isWhitespace)


(>>=) = flip Parser.andThen


pName : Parser x String
pName = 
  Parser.satisfy isAlphaNumLower >>= \c ->
  Parser.many (Parser.satisfy isAlphaNum) >>= \cs ->
  whitespace >>= \_ ->
  let name = String.fromList (c::cs) in 
  if name == "let" then 
    Parser.fail ""
  else 
    Parser.succeed name


pVar : Parser x (Term ())
pVar =
  pName >>= \name ->
  whitespace >>= \_ ->
  Parser.succeed (Var () name)


pAbs : Parser x (Term ())
pAbs =
  Parser.satisfy ( (==) '\\') >>= \_ ->
  whitespace >>= \_ ->
  pName >>= \name ->
  whitespace >>= \_ ->
  Parser.satisfy ( (==) '.') >>= \_ ->
  whitespace >>= \_ ->
  pTerm >>= \term ->
  whitespace >>= \_ ->
  Parser.succeed (Abs () name term)
  

pApp : Parser x (Term ())
pApp =
  Parser.satisfy ( (==) '@') >>= \_ ->
  whitespace >>= \_ ->
  pTerm >>= \term1 ->
  whitespace >>= \_ ->
  pTerm >>= \term2 ->
  whitespace >>= \_ ->
  Parser.succeed (App () term1 term2)


pLet : Parser x (Term ())
pLet =
  Parser.satisfy ( (==) 'l') >>= \_ ->
  Parser.satisfy ( (==) 'e') >>= \_ ->
  Parser.satisfy ( (==) 't') >>= \_ ->
  Parser.satisfy isWhitespace >>= \_ ->
  whitespace >>= \_ ->
  pName >>= \name ->
  whitespace >>= \_ ->
  Parser.satisfy ( (==) '=') >>= \_ ->
  whitespace >>= \_ ->
  pTerm >>= \term1 ->
  whitespace >>= \_ ->
  pTerm >>= \term2 ->
  whitespace >>= \_ ->
  Parser.succeed (App () (Abs () name term2) term1)


pTerm : Parser x (Term ())
pTerm = 
  whitespace >>= \_ ->
  ( pLet |> Parser.or pVar |> Parser.or pAbs |> Parser.or pApp )

