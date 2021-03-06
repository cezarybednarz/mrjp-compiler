{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Latte.
--   Generated by the BNF converter.

module Latte.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, dropWhile, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Latte.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i = \case
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt     _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Latte.Abs.Ident where
  prt _ (Latte.Abs.Ident i) = doc $ showString i

instance Print (Latte.Abs.Program' a) where
  prt i = \case
    Latte.Abs.Program _ topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print (Latte.Abs.TopDef' a) where
  prt i = \case
    Latte.Abs.FnDef _ type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Latte.Abs.TopDef' a] where
  prt = prtList

instance Print (Latte.Abs.Arg' a) where
  prt i = \case
    Latte.Abs.Arg _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Latte.Abs.Arg' a] where
  prt = prtList

instance Print (Latte.Abs.Block' a) where
  prt i = \case
    Latte.Abs.Block _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [Latte.Abs.Stmt' a] where
  prt = prtList

instance Print (Latte.Abs.Stmt' a) where
  prt i = \case
    Latte.Abs.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    Latte.Abs.BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    Latte.Abs.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    Latte.Abs.Ass _ lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    Latte.Abs.Incr _ lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    Latte.Abs.Decr _ lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    Latte.Abs.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    Latte.Abs.VRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    Latte.Abs.Cond _ expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Latte.Abs.CondElse _ expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    Latte.Abs.ForEach _ type_ id_ expr stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 type_, prt 0 id_, doc (showString ":"), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Latte.Abs.While _ expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    Latte.Abs.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Latte.Abs.Item' a) where
  prt i = \case
    Latte.Abs.NoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    Latte.Abs.Init _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Latte.Abs.Item' a] where
  prt = prtList

instance Print (Latte.Abs.Type' a) where
  prt i = \case
    Latte.Abs.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    Latte.Abs.Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    Latte.Abs.Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    Latte.Abs.Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    Latte.Abs.Array _ type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    Latte.Abs.Fun _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Latte.Abs.Type' a] where
  prt = prtList

instance Print (Latte.Abs.LValue' a) where
  prt i = \case
    Latte.Abs.LVar _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    Latte.Abs.LIdx _ expr1 expr2 -> prPrec i 0 (concatD [prt 6 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])

instance Print (Latte.Abs.Expr' a) where
  prt i = \case
    Latte.Abs.ELitInt _ n -> prPrec i 7 (concatD [prt 0 n])
    Latte.Abs.ELitTrue _ -> prPrec i 7 (concatD [doc (showString "true")])
    Latte.Abs.ELitFalse _ -> prPrec i 7 (concatD [doc (showString "false")])
    Latte.Abs.ELValue _ lvalue -> prPrec i 6 (concatD [prt 0 lvalue])
    Latte.Abs.EApp _ id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Latte.Abs.EString _ str -> prPrec i 6 (concatD [prt 0 str])
    Latte.Abs.ELength _ expr -> prPrec i 5 (concatD [prt 6 expr, doc (showString "."), doc (showString "length")])
    Latte.Abs.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Latte.Abs.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    Latte.Abs.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    Latte.Abs.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    Latte.Abs.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    Latte.Abs.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    Latte.Abs.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    Latte.Abs.ENewArr _ type_ expr -> prPrec i 0 (concatD [doc (showString "new"), prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Latte.Abs.Expr' a] where
  prt = prtList

instance Print (Latte.Abs.AddOp' a) where
  prt i = \case
    Latte.Abs.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Latte.Abs.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Latte.Abs.MulOp' a) where
  prt i = \case
    Latte.Abs.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    Latte.Abs.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    Latte.Abs.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (Latte.Abs.RelOp' a) where
  prt i = \case
    Latte.Abs.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    Latte.Abs.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    Latte.Abs.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    Latte.Abs.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    Latte.Abs.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    Latte.Abs.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])

