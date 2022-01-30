# Latte language compiler
### Author: Cezary Bednarz 

## About
Compiler of Java-like Latte language. Syntax in BNF form is defined in `src/Latte.cf`. Examples can be found in `test` folder.

## Tools
Compiler is written in Haskell.
Parser is generated by BNFC version 2.9.1 using grammar defined in file `src/Latte.cf` and was generated using following command:
`bnfc --haskell --functor -d -o src Latte.cf`
All used modules are defined in `package.yaml` in the main directory.

## Running 
 - `make`: builds compiler
 - `./latc_llvm <path to Latte file>`: compiles input file and outputs it .bc and .ll file in the same directory.
 - `lli file.bc` executes compiled latte program

## Optimizations 
 - `&&` and `||` evaluation using jump method
 - mem2reg: transformation to SSA form using "Modern SSA Algorithm"
 - LCSE: local common subexpression elimination, executes while running GCSE optimization
 - GCSE: global common subexpression elimination

## Extensions 
 - arrays with foreach loop and `.length`

## File structure
 - `lib/runtime.bc`: runtime library with functions like printing and string concatenation
 - `src/`
   - `Main.hs`: gets input file, performs semantic analysis and compiles it
   - `Frontend/`: module which executes semantic analysis
   - `Backend/`: module which compiles AST to llvm IR code
   - `Common/`: module with common definitions betwen Backend and Frontend
   - `Optimizations/`: contains mem2reg and GCSE optimizations that run on LLVM IR code. `Mem2Reg.hs` file contains trivial phi removal algorithm which is also required after GCSE optimization
   - `Latte/`: folder with parser files generated by BNFC
