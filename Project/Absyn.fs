module Absyn

open Environ

type plcType =
  | IntT                                (*  Int                     *)
  | BooT                                (*  Bool                    *)
  | FunT of plcType * plcType           (*  type -> type            *)
  | TupT of plcType list                (*  Unit
                                            Tuple[type, ..., type]  *)
  | LisT of plcType                     (*  List[type]              *)

type expr = 
  | ConI of int
  | ConB of bool
  | EList of plcType
  | Var of string
  | Let of string * expr * expr
  | Letrec of string * string * plcType * expr * plcType * expr
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | If of expr * expr * expr
  | Call of expr * expr
  | Tuple of expr list
  | Sel of expr * int
  | Anon of string * plcType * expr


type plcVal = 
  | BooV of bool
  | IntV of int
  | LisV of plcVal list
  | TupV of plcVal list
  | Clos of string * string * expr * plcVal env 


let list2string conv sep l =
  let conc s t = s + sep + conv t in
  match l with 
  | [] -> ""
  | v :: vs -> List.fold conc (conv v) vs

let rec type2string t =
  match t with
  | BooT -> "Bool"
  | IntT -> "Int"
  | TupT [] -> "Unit"
  | TupT ts -> "Tuple[" + list2string type2string ", " ts + "]" 
  | LisT t1 -> "List[" + type2string t1 + "]"
  | FunT (t1, t2) -> 
    match t1 with 
    | FunT _ -> "(" + type2string t1 + ") -> " + type2string t2
    | _      ->       type2string t1 +  " -> " + type2string t2

let rec val2string v =
  match v with
  | BooV true -> "true"
  | BooV false -> "false"
  | IntV n -> string n
  | LisV vs -> "[" + list2string val2string "; " vs + "]"
  | TupV vs -> "(" + list2string val2string ", " vs + ")" 
  | Clos _ -> "<fun>" 
