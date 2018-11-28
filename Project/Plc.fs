
module Plc

open Absyn
open PlcInterp
open PlcChecker

let run (e: expr) = 
  let t = teval e [] in
  let v = eval e [] in
  (val2string v) + " : " + (type2string t)

