
module PlcInterp

open Absyn
open Environ

let rec eval (e : expr) (env : plcVal env) : plcVal =
    // to be implemented
    match e with // all temp
    | ConI i -> IntV i
    | ConB b -> BooV b

    | Var x  ->
      let v = lookup env x in
      match v with
      | IntV  _ -> v
      | BooV _ -> v 
      | TupV  _ -> v 
      | _      -> failwith ("Value of variable _" + x + "_ is not first-order.")

    | EList _ -> TupV []
    
    | Prim1 (op, e1) -> 
      let v1 = eval e1 env in
      match (op, v1) with
      | ("-", IntV i) -> IntV (- i)
      | ("!", BooV b) -> BooV (not b)
      | ("hd", LisV a) -> a.Head
      | ("tl", LisV a) -> LisV a.Tail
      | ("print", _) -> printfn "%s" (val2string v1); TupV []
      | ("ise", LisV a) -> BooV (a.IsEmpty)
      | _   -> failwith "Impossible"

    | Prim2 (op, e1, e2) -> 
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | ("=", _, _) -> BooV (v1 = v2)
      | ("!=", _, _) -> BooV (not (v1 = v2))
      | ("<", IntV i1, IntV i2) -> BooV (i1 < i2)
      | ("<=", IntV i1, IntV i2) -> BooV (i1 <= i2)
      | ("*", IntV i1, IntV i2) -> IntV (i1 * i2)
      | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)
      | ("+", IntV i1, IntV i2) -> IntV (i1 + i2)
      | ("-", IntV i1, IntV i2) -> IntV (i1 - i2)
      | ("&&", BooV i1, BooV i2) -> BooV (i1 && i2)
      | ("::", t1, LisV t2) -> LisV (t1 :: t2)
      | (";", _, _) -> v2
      | _   -> failwith "Impossible"
    
    | Let (x, e1, e2) -> 
      let v = eval e1 env in
      let env2 = (x, v) :: env in
      eval e2 env2
    
    | If (e1, e2, e3) -> 
      let v1 = eval e1 env in
      match v1 with
      | BooV true  -> eval e2 env
      | BooV false -> eval e3 env
      | _ -> failwith "Impossible"

    | Anon (f, _, e1) -> Clos("", f, e1, env)

    | Letrec (name, var, vartype, ex1, rettype, ex2) -> 
      let env2 = (name, Clos(name, var, ex1, env)) :: env in
      eval ex2 env2

    | Call (Var f, e) -> 
      let c = lookup env f in
      match c with
      | Clos (f, x, e1, fenv) ->
        let v = eval e env in
        let env1 = (x, v) :: (f, c) :: fenv in
        eval e1 env1
      | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"

    | Tuple es -> TupV (List.map (fun e -> eval e env) es)

    | Sel (e1, n) -> 
      match eval e1 env with 
      | TupV vs -> List.item (n - 1) vs
      | _ -> failwith "Impossible"

