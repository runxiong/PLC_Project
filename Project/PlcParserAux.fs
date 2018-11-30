module ParAux

open Absyn
let tup =  "$tuple"
let etup =  "()"
let rec makeFunAux (n: int) (xs: (string * plcType) list) (e: expr) : expr = 
  match xs with
  | []     -> e
  | (x, t) :: r -> Let (x, Sel (Var tup, n), makeFunAux (n + 1) r e)  

let makeType (l: (string * plcType) list): plcType = TupT (List.map (fun (x,y) -> y) l)

let makeFun (f: string) (xs: (string * plcType) list) (rt: plcType) (e1: expr) (e2: expr) : expr =
  match xs with
  | []           -> Letrec (f, etup, TupT [], e1, rt, e2)   
  | (x, t) :: [] -> Letrec (f, x, t, e1, rt, e2)
  | _            -> 
    let t = makeType xs in
    let e1' = makeFunAux 1 xs e1 in
    Letrec (f, tup, t, e1', rt, e2)

let rec makeFunCurried (f: string) (xslist: ((string * plcType) list) list) (rt: plcType) (e1: expr) (e2: expr) : expr =
  match xslist with
  | []        -> failwith "Miss necessary argument"
  | t :: []   -> makeFun f t rt e1 e2
  | t :: n    -> makeFunCurried f n rt (makeFun f t rt e1 e2) e2

let makeAnon (xs: (string * plcType) list ) (e: expr) : expr =
  match xs with 
  | []           -> Anon (etup, TupT [], e)   
  | (x, t) :: [] -> Anon (x, t, e)
  | _            -> 
   let t = makeType xs in
   let e' = makeFunAux 1 xs e in
   Anon (tup, t, e')

let rec foldR f l x =
  match l with
  | []     -> x
  | h :: t -> f h (foldR f t x)

let makeAnonCurried (xslist : ((string * plcType) list) list ) (e : expr) : expr = 
  foldR makeAnon xslist e

let makeExpr (a : dec) (b : expr) : expr =
  match a  with
   | A (t1, t2) -> Let (t1, t2, b)
   | B (t1, t2, t3) -> Let (t1, makeAnonCurried t2 t3, b)
   | C (t1, t2, t3,t4) -> makeFun t1 t2 t3 t4 b
