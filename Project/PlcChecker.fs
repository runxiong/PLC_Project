
module PlcChecker

open Absyn
open Environ

// The type checker can be seen as an interpreter that computes
// the type of an expression instead of its value.

let rec teval (e : expr) (env : plcType env) : plcType =
    // to be implemented 
    match e with // temp
    | ConI i -> IntT 

    | ConB b -> BooT
  
    | Var x  -> lookup env x 

    | EList l -> l
  
    | Prim1 (op, e1) -> 
      let t1 = teval e1 env in
      match (op, t1) with
      | ("-", IntT) -> IntT
      | ("!", BooT) -> BooT
      | ("hd", LisT a) -> a
      | ("tl", LisT a) -> LisT a
      | ("print", _) -> TupT []
      | ("ise", LisT _) -> BooT
      | _   -> failwith "Checker: unknown op, or type error"

    | Prim2 (op, e1, e2) -> 
      let t1 = teval e1 env in
      let t2 = teval e2 env in
      match (op, t1, t2) with
      | ("*", IntT, IntT) -> IntT
      | ("/", IntT, IntT) -> IntT
      | ("+", IntT, IntT) -> IntT
      | ("-", IntT, IntT) -> IntT
      | ("<", IntT, IntT) -> BooT
      | ("<=", IntT, IntT) -> BooT   
      | ("&&", BooT, BooT) -> BooT
      | ("=", BooT, BooT) -> BooT   // not sure if this actually covers all cases; should = and != apply between nonequivalent types?    
      | ("=", IntT, IntT) -> BooT                      
      | ("=", TupT [], TupT []) -> BooT
      | ("!=", BooT, BooT) -> BooT                      
      | ("!=", IntT, IntT) -> BooT                      
      | ("!=", TupT [], TupT []) -> BooT
      | ("::", t1, LisT t2) -> if t2 = t1 then LisT t2 else
                                            match t2 with
                                            | Hold -> LisT t1
                                            | _ -> failwith ("Checker: type error in list operator :: with types " + type2string t1 + " and " + type2string t2)
      | (";", _, _) -> t2
      | _   -> failwith "Checker: unknown op, or type error"

    | Let (x, e1, e2) -> 
      let t = teval e1 env in
      let env' = (x, t) :: env in
      teval e2 env'
  
    | If (e1, e2, e3) -> 
      match teval e1 env with
      | BooT -> let t2 = teval e2 env in
                 let t3 = teval e3 env in
                 if t2 = t3 then 
                   t2
                 else 
                   failwith "Checker: 'if' branch types differ"

      | _    -> failwith "Checker: 'if':' condition not Boolean"
 
    | Anon (f, t, e1) -> 
        let fenv = (f, t) :: env in
        let rettype = (teval e1 fenv) in
        FunT(t, rettype)

    | Letrec (name, var, vartype, ex1, rettype, ex2) -> 
      let vart = FunT (vartype, rettype) in
      let fenv = (var, vartype) :: (name, vart) :: env in
      let lenv = (name, vart) :: env in
      let e1type = teval ex1 fenv in
      if e1type = rettype then teval ex2 lenv else 
      match e1type with
      | FunT(a, b) -> if b = rettype then teval ex2 lenv 
                                     else failwith ("Checker: Incorrect return type from FunT in function " + name)
      | _ -> failwith ("Checker: wrong return type in function " + name)
        // if teval ex2 lenv = rettype then
        //     FunT(vartype, rettype)
        // else
        //   failwith ("Checker: wrong return type in function " + name)
         // FunT(vartype, rettype)
    
    (*| Letfun (f, x, xt, e1, rt, e2) -> 
      let ft = FunT (xt, rt) in
      let fenv = (x, xt) :: (f, ft) :: env in
      let lenv = (f, ft) :: env in
      if teval e1 fenv = rt then 
        teval e2 lenv
      else
        failwith ("Checker: wrong return type in function " + f) *)
 
    | Call (n, e) ->
      match n with
      | Var f -> 
        match lookup env f with
        | FunT (xt, rt) ->
          if teval e env = xt then 
            rt
          else
            failwith "Checker: type mismatch in function call.."
        | _ -> failwith ("Checker: function " + f + " is undefined")
      | Call _ ->
        let c = teval n env in
        match c with 
        | FunT (xt, rt) ->
          if teval e env = xt then
            rt
          else
            failwith "Checker: type mismatch in funciton call"
        | _ -> c // failwith ("Checker: Expected function but got " + type2string c)

      | _ -> failwith "Call: illegal function in call"
 
    //| Call _ -> failwith "Call: illegal function in call"

    | Tuple es -> TupT (List.map (fun e -> teval e env) es)

    | Sel (e1, n) -> 
      match teval e1 env with 
      | TupT ts -> 
        if 0 < n && n <= List.length ts then
          List.item (n - 1) ts
        else
          failwith "Checker: Tuple index out of range"
      | _ -> failwith ("Checker: selection operator #" + string n + "applied to non-tuple")