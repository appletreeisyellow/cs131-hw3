(* Name: 

   UID: 

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern. If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with 
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> 
        Env.empty_env()
    | (BoolPat(x), BoolVal(y)) when x=y -> 
        Env.empty_env()
    | (WildcardPat, _) -> 
        Env.empty_env()
    | (VarPat(s), _) -> 
        (Env.add_binding s value (Env.empty_env())) 
    | (TuplePat(lst1), TupleVal(lst2)) -> (
        match (lst1, lst2) with
          ([], []) -> Env.empty_env()
        | (h1::t1, h2::t2) -> (Env.combine_envs (patMatch h1 h2) (patMatch (TuplePat t1) (TupleVal t2)))
        | (_::_, []) -> raise(MatchFailure)
        | ([], _::_) -> raise(MatchFailure)) 
    | (DataPat(s1, pattern), DataVal(s2, value)) -> (
        match pattern with
        None -> (
          match value with 
          None -> if s1=s2 then Env.empty_env() else raise(MatchFailure)
          | Some v -> raise(MatchFailure))
        | Some p -> (
          match value with
          None -> raise(MatchFailure) 
          | Some v -> if s1=s2 then (patMatch p v) else raise(MatchFailure)))
    | _ -> raise(MatchFailure)


    

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i)     -> IntVal(i)
    | BoolConst(x)  -> BoolVal(x)
    | Var(s)        -> (
        try (Env.lookup s env) with 
        Env.NotBound -> raise(DynamicTypeError "unbounded variable"))
    | BinOp(exp1, op, exp2) -> (
        let v1 = evalExpr exp1 env in 
        let v2 = evalExpr exp2 env in 
        match (v1, v2) with 
        (IntVal v1, IntVal v2) -> 
        (match op with 
          Plus -> IntVal(v1 + v2)
          | Minus -> IntVal(v1 - v2)
          | Times ->  IntVal(v1 * v2)
          | Eq -> BoolVal(v1 = v2)
          | Gt -> BoolVal(v1 > v2))
        | _ -> raise (DynamicTypeError "operation can only applied to integers"))
    | Negate(e0)      -> (
        let v0 = evalExpr e0 env in
        match v0 with 
        IntVal(i) -> IntVal(-i) 
        | _ -> raise(DynamicTypeError "can only negate integers"))
    | If(e1, e2, e3)  -> (
        let v1 = try evalExpr e1 env with 
        Env.NotBound -> raise(DynamicTypeError "unbounded variable") in
        match v1 with
        BoolVal(b) -> 
          if b=true 
          then evalExpr e2 env 
          else evalExpr e3 env
        | _ -> raise(DynamicTypeError "the first expression should return bool"))
    | Function(param, funcBody) -> FunctionVal(None, param, funcBody, env)
    | FunctionCall(exp1, exp2)  -> (
        let myFunction = evalExpr exp1 env in 
        let argum = evalExpr exp2 env in
        match myFunction with 
          FunctionVal(funcName, param, funcBody, funEnv) -> (
            let aEnv = patMatch param argum in 
            match funcName with
            None -> (let currentEnv = Env.combine_envs funEnv aEnv in 
            evalExpr funcBody currentEnv)
            | Some name -> (let funcBodyFromGlobal = Env.lookup name env in 
              let recFunEnv = Env.add_binding name funcBodyFromGlobal (Env.empty_env()) in 
            let currentEnv = Env.combine_envs (Env.combine_envs funEnv recFunEnv) aEnv in 
            evalExpr funcBody currentEnv))
          | _ -> raise(MatchFailure))
    | Match(exp, lst) -> ( 
        let expression = evalExpr exp env in 
        match lst with 
          [] -> raise(MatchFailure)
        | (pat', exp') :: t -> (
          try let currentEnv = Env.combine_envs env (patMatch pat' expression) in
          evalExpr exp' currentEnv 
          with MatchFailure -> (evalExpr (Match(exp, t)) env)))
    | Tuple(lst) -> (
        match lst with 
        [] -> TupleVal([])
        | [x] -> evalExpr x env
        | [e1; e2] -> TupleVal((evalExpr e1 env) :: [evalExpr e2 env] )
        | h :: t -> 
          let v = evalExpr h env in 
          let restVal = evalExpr (Tuple t) env in 
          match restVal with 
            TupleVal(v1::vr) -> TupleVal(v::v1::vr))
          (*| (IntVal _|BoolVal _|FunctionVal (_, _, _, _)|DataVal (_, _)) *)
    | Data(s, expOpt) -> (
        match expOpt with
          None -> DataVal(s, None)
        | Some x -> DataVal(s, Some (evalExpr x env)))
    | _ -> raise MatchFailure
     
 (*   
      prerr_string ("x" ^ "y");;
      prerr_string(string_from_int x);;
*)

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | Let(s, e) -> (Some s, evalExpr e env) (* let x = e *)
    | LetRec(s, e) -> (
        let func = evalExpr e env in 
        match func with
          FunctionVal(_, pattern', exp', env') ->
            (Some s, FunctionVal(Some s, pattern', exp', env')) (* let rec f x = e *)
        | _ -> raise(MatchFailure)) 













