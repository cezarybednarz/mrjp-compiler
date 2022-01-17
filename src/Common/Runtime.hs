module Common.Runtime where

import           Latte.Abs as Latte

libraryFunctions :: BNFC'Position -> [TopDef]
libraryFunctions l =
  [ FnDef l (Void l) (Ident "printInt") [Arg l (Int l) (Ident "n")] (Block l []),
    FnDef l (Void l) (Ident "printString") [Arg l (Str l) (Ident "s")] (Block l []),
    FnDef l (Int l) (Ident "readInt") [] (Block l [Ret l (ELitInt l 0)]),
    FnDef l (Str l) (Ident "readString") [] (Block l [Ret l (EString l "")]),
    FnDef l (Void l) (Ident "error") [] (Block l []),
    FnDef l (Int l) (Ident "__equString__") [Arg l (Str l) (Ident "a"), Arg l (Str l) (Ident "b")] (Block l [Latte.Ret l (ELitInt l 0)]),
    FnDef l (Str l) (Ident "__concatStrings__") [Arg l (Str l) (Ident "a"), Arg l (Str l) (Ident "b")] (Block l [Latte.Ret l (EString l "")])]
