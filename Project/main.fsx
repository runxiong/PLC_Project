open Parse
open Parse
open Parse
(* 
  Make sure you regenerate the Parser and Lexer
  every time you modify PlcLexer.fsl or PlcParser.fsy
*)

(*
// Windows only
   #r "H:\Desktop\Project\\bin\FsLexYacc.Runtime.dll"
   #load "H:\Desktop\Project\Environ.fs" 
   #load "H:\Desktop\Project\Absyn.fs" 
   #load "H:\Desktop\Project\PlcParserAux.fs"
   #load "H:\Desktop\Project\PlcParser.fs"
   #load "H:\Desktop\Project\PlcLexer.fs" 
   #load "H:\Desktop\Project\Parse.fs" 
   #load "H:\Desktop\Project\PlcInterp.fs"
   #load "H:\Desktop\Project\PlcChecker.fs"
   #load "H:\Desktop\Project\Plc.fs"
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

let e13 = fromString
"fun inc (x : Int) = x + 1;
fun add (x : Int, y : Int) = x + y;
fun highAdd (x : Int) = fn (y : Int) => x + y end;
var y = add(3, inc(4));
var x = highAdd(3)(7-y);
var z = x * 3;
fun rec fact (n : Int) : Int =
  if n = 0 then 1 else n * fact(n - 1);
print x; print y;
x :: y :: z :: fact(z) :: ([] : List[Int]) "

let e14 = fromString "x :: y :: z :: fact(z) :: ([] : List[Int])"

let e15 = fromString 
"var E = ([] : List[Int]);
fun reverse (l : List[Int]) = {
  fun rec rev (l1 : List[Int], l2 : List[Int]): List[Int] =
    if ise(l1) then
l2 else {
      var h = hd(l1);
      var t = tl(l1);
      rev(t, h::l2)
}; rev(l, E)
};
reverse (1::2::3::E)"

let e16 = fromString
"fun twice (f : Int -> Int) = fn (x : Int) => f(f(x)) end ;
fun rec map (f : Int -> Int) : (List[Int] -> List[Int]) =
  fn (l: List[Int]) =>
    if ise(l) then l else f(hd(l)) :: map(f)(tl(l))
  end ;
fun square (x : Int) = x * x ;
fun inc (x : Int) = x + 1 ;
var E = ([] : List[Int]) ;
var l1 = map (fn (x:Int) => 2*x end) (10::20::30::E) ;
var l2 = map (twice(inc)) (l1) ;
(l1, l2)"

// Extra Creidt I
let e17a = fromString "[e1]"
let e17b = fromString "e1 :: ([] : List[Int])"
let e18a = fromString "[e1; e2]"
let e18b = fromString "e1 :: e2 :: ([] : List[Int])"
let e19a = fromString "[e1; e2; e3]"
let e19b = fromString "e1 :: e2 :: e3 :: ([] : List[Int])"