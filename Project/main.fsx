open Parse
(* 
  Make sure you regenerate the Parser and Lexer
  every time you modify PlcLexer.fsl or PlcParser.fsy
*)

// Mac Os ony
// run fsharpi in Project dir
#r "bin/FsLexYacc.Runtime.dll"
   #load "Environ.fs"
   #load "Absyn.fs"
   #load "PlcParserAux.fs"
   #load "PlcParser.fs"
   #load "PlcLexer.fs"
   #load "Parse.fs"
   #load "PlcInterp.fs"
   #load "PlcChecker.fs"
   #load "Plc.fs"

open Absyn
let fromString = Parse.fromString // string parser function
let run e = printfn "\nResult is  %s\n" (Plc.run e)   // execution function


(* Examples in concrete syntax *)
let e1 = fromString " 15 "

let e2 = fromString " true  "

let e3 = fromString " () "

let e4 = fromString " (6, false) "

let e5 = fromString " (6, false)#1 "

let e6 = fromString "([]:List[Bool])"

let e7 = fromString " print x; true "

let e8 = fromString " 3::7::t "

let e9 = fromString "fn (x:Int) => -x end"

let e10 = fromString "var x = 9; x + 1"

let e11 = fromString "fun f(x:Int) = x; f(1)"

let e12 = fromString "fun rec f(n:Int) : Int = if n <= 0 then 0 else n + f(n-1) ; f(5)"

let e13 = fromString " List 5 "