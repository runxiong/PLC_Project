open Test
open Parse
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
let interp = PlcInterp.eval
let typecheck = PlcChecker.teval

(* Examples in concrete syntax *)
let e1 = fromString " 15 "
run e1
interp e1 []
typecheck e1 []
let e2 = fromString " true  "
run e2
interp e2 []
typecheck e2 []

let e3 = fromString " () "
run e3
interp e3 []
typecheck e3 []
let e4 = fromString " (6, false) "
run e4
interp e4 []
typecheck e4 []

let e5 = fromString " (6, false)#1 "
run e5
interp e5 []
typecheck e5 []
let e6 = fromString "([]:List[Bool])"
run e6
interp e6 []
typecheck e6 []

let e7 = fromString " var x = 3; print x; true "
run e7
interp e7 []
typecheck e7 []

let e8e = fromString  " 3; 2; 4; fact(x); e1; e2; e3 "

let e8a = fromString " 3 :: 6 :: 7 :: ([]:List[Int] ) "
run e8a
interp e8a []
typecheck e8a []
let e8 = fromString " var t = 5; 3::7::t :: ([]:List[Int] ) "
run e8
interp e8 []
typecheck e8 []

let e9 = fromString "fn (x:Int) => -x end"
run e9
interp e9 []
typecheck e9 []

let e10 = fromString "var x = 9; x + 1"
run e10
interp e10 []
typecheck e10 []

let e11 = fromString "fun f(x:Int, y:Int, z:Int) = x + y + z; f(1, 2, 3)"
run e11
interp e11 []
typecheck e11 []


let e11a = fromString "fun f(x:Int) (y:Int) (z:Int) = x + y + z; f 1 2 3"
run e11a
interp e11a []
typecheck e11a []

let e11b = fromString "fun rec move (l1 : List[Int]) : List[Int] = fn (l2 : List[Int]) => 
if ise(l1) then l2 else move (tl(l1)) ( hd(l1)::l2 ) end;  move (1 :: 2 :: 3 :: ([]:List[Int] )) (4 :: 5 :: ([]:List[Int]) ) "
run e11b
interp e11b []
typecheck e11b []

let e11c = fromString "fun rec move (l1 : List[Int]) (l2 : List[Int]) : List[Int] =
if ise(l1) then l2 else move (tl(l1)) ( hd(l1)::l2 );  move (1 :: 2 :: 3 :: ([]:List[Int] )) (4 :: 5 :: ([]:List[Int]) ) "
run e11c
interp e11c []
typecheck e11c []

let e12 = fromString "fun rec f(n:Int) : Int = if n <= 0 then 0 else n + f(n-1) ; f(5)"
run e12
interp e12 []
typecheck e12 []


let e13 = fromString "fun inc (x : Int) = x + 1;
fun add (x : Int, y : Int) = x + y;
fun highAdd (x : Int) = fn (y : Int) => x + y end;
var y = add(3, inc(4));
var x = highAdd(3)(7-y);
var z = x * 3;
fun rec fact (n : Int) : Int =
  if n = 0 then 1 else n * fact(n - 1);
print x; print y;
x :: y :: z :: fact(z) :: ([] : List[Int]) "

run e13
interp e13 []
typecheck e13 []

let e14 = fromString "x :: y :: z :: fact(z) :: ([] : List[Int])"
let e14a = fromString "[x; y; z; fact(z)]"


let e15 = fromString "var E = ([] : List[Int]);
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
run e15
interp e15 []
typecheck e15 []
let e16 = fromString "fun twice (f : Int -> Int) = fn (x : Int) => f(f(x)) end ;
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
run e16
interp e16 []
typecheck e16 []


let e16a = fromString "fun rec move (l1 : List[Int])  : List[Int] = hd(l1); move(l)"

// Extra Credit I
let e17a = fromString "[3+3]"
let e17b = fromString "e1 :: ([] : List[Int])"
let e18a = fromString "[e1; e2]"
let e18b = fromString "e1 :: e2 :: ([] : List[Int])"
let e19a = fromString "[e1; e2; e3]"
let e19b = fromString "[ e1 :: e2 :: e3 :: ([] : List[Int]) ]"
let e20 = fromString " [[e1]; [e2]] "

// Extra Credit II
let e21 = fromString "fn (x:Int) (y : Int) (z : Int) => x + y + z end " 
let e22 = fromString " fun rec f (x:Int) : Int = if x=0 then 0 else 1 + f(x-1); f(5)"
let e23 = fromString "var highAdd = fn (x:Int) (y:Int) => x + y end ; highAdd 7 8"
run e23
let e24 = fromString " fun leq (x:Int) (y:Int) = x <= y ; 8"
let e25 = fromString " 1 - 3; {var x = 4; 2 * x} "
let e26 = fromString "var highAdd = fn (x:Int) => fn (y:Int) => x + y end end; 8"

printf "5.5"

// let rec f x = fun y -> if x > 0 then f (x-1) (y-1) + y else y

// let rec f = fun x -> fun y -> if x > 0 then 1+ f (x-1) (y-1) else y

let rec map f l = 
  match l with
    | []     -> []
    | h :: t -> (f h) :: (map f t)
#load "Test.fs"
let cases = map snd Test.cases

List.length cases
let ty = map (fun x-> typecheck x [] ) cases
typecheck cases.[0] []
typecheck cases.[1] []
typecheck cases.[2] []
typecheck cases.[3] []
typecheck cases.[4] []
typecheck cases.[5] []
typecheck cases.[6] []
typecheck cases.[7] []
typecheck cases.[8] []
typecheck cases.[9] []
typecheck cases.[10] []
typecheck cases.[11] []
typecheck cases.[12] []
typecheck cases.[13] []
typecheck cases.[14] []
typecheck cases.[15] []
typecheck cases.[16] []
typecheck cases.[17] []
typecheck cases.[18] []
typecheck cases.[19] []
typecheck cases.[20] []
typecheck cases.[21] []
typecheck cases.[22] []
typecheck cases.[23] []
typecheck cases.[24] []
typecheck cases.[25] []
typecheck cases.[26] []
typecheck cases.[27] []
typecheck cases.[28] []
typecheck cases.[29] []
typecheck cases.[30] []
typecheck cases.[31] []
typecheck cases.[32] []
typecheck cases.[33] []
typecheck cases.[34] []
typecheck cases.[35] []
typecheck cases.[36] []
typecheck cases.[37] []
typecheck cases.[38] []
typecheck cases.[39] []
typecheck cases.[40] []
typecheck cases.[41] []
typecheck cases.[42] []
typecheck cases.[43] []
typecheck cases.[44] []
typecheck cases.[45] []
typecheck cases.[46] []
typecheck cases.[47] []
typecheck cases.[48] []
typecheck cases.[49] []
typecheck cases.[50] []
typecheck cases.[51] []
typecheck cases.[52] []
typecheck cases.[53] []
typecheck cases.[54] []
typecheck cases.[55] []
typecheck cases.[56] []

run cases.[0]
run cases.[1]
run cases.[2]
run cases.[3]
run cases.[4]
run cases.[5]
run cases.[6]
run cases.[7]
run cases.[8]
run cases.[9]
run cases.[10]
run cases.[11]
run cases.[12]
run cases.[13]
run cases.[14]
run cases.[15]
run cases.[16]
run cases.[17]
run cases.[18]
run cases.[19]
cases.[20]
run cases.[20]
cases.[21]
run cases.[21]
cases.[22]
run cases.[22]
run cases.[23]
run cases.[24]
run cases.[25]
run cases.[26]
run cases.[27]
run cases.[28]
run cases.[29]
run cases.[30]
run cases.[31]
run cases.[32]
run cases.[33]
run cases.[34]
run cases.[35]
run cases.[36]
run cases.[37]
run cases.[38]
run cases.[39]
run cases.[40]
run cases.[41]
run cases.[42]
run cases.[43]
run cases.[44]
run cases.[45]
run cases.[46]
run cases.[47]
run cases.[48]
run cases.[49]
run cases.[50]
run cases.[51]
run cases.[52]
run cases.[53]
run cases.[54]
run cases.[55]
run cases.[56]

let rec move a b = 
  match a with
    [] -> b
  | _ -> move (List.tail a) ((List.head a)::b)

move [1;2;3] [4;5]

let rec mov a = fun b -> 
    match a with
    [] -> b
  | _ -> mov (List.tail a) ((List.head a)::b)

move [1;2;3] [4;5]

let m = move [1;2]
m [4 ]
m
let m1 = mov [1;2]
m1