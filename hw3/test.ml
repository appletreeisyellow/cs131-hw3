
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result:
     - the MOCaml declaration is a string of exactly what you would type into the interpreter prompt,
       without the trailing ";;"
     - the expected result is a string of exactly what you expect the interpreter to print as a result
   use the string "dynamic type error" as the result if a DynamicTypeError is expected to be raised.
   use the string "match failure" as the result if a MatchFailure is expected to be raised.
   use the string "implement me" as the result if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
    (* YOU NEED TO ADD A LOT MORE TESTS! *)
		("3", "3"); 
		("false", "false");
		("let x = 34", "val x = 34");
    (* Val(s) *)
    ("x", "34");
    ("y", "dynamic type error");
    (* BinOp *)
    ("9 + false", "dynamic type error");
    ("3 + 6", "9");
    ("2 - 4", "-2");
    ("4 * 8", "32");
    ("1 = 3", "false");
    ("10 = 10", "true");
    ("false = false", "dynamic type error");
    ("2 > 3", "false");
    ("2 > 1", "true");
    ("false > 2", "dynamic type error");
    ("y + 4", "dynamic type error");
    ("x + 4", "38");
    (* negate *)
    ("-(-2)", "2"); 
    ("-(false)", "dynamic type error");
    ("-(x)", "-34");
    ("-(y)", "dynamic type error");
    (* If *)
    ("if x=100 then true else false", "false");
    ("if x>y then x+2 else false", "dynamic type error");
    ("if 2>x then x+2 else false", "false");
    ("if x>2 then x+2 else false", "36");
    (* Function *)
    ("(function x -> x+2) 3", "5");
    ("(function false -> 3+6) false", "9");
    (* FunctionCall *)
    ("let rec f num = if num > 0 then true else x (num+1)", "val f : int -> bool = <fun>");
    ("f 1", "true");
    ("f (-1)", "true");

    (*
let rec double i = i*2;;
let rec twice f = function x -> f(f(x));;
(twice double) 10;;

should return 40
    *)

    (* 
let x = 3;;
let rec f y = x+y;;
let x = 5;;
x + (f 2);; (* should return 10 *)

mocaml# Leaf;;
Leaf
mocaml# Node 1;;
Node 1
mocaml# Node (true,1,Leaf);;
Node (true,1,Leaf);;
*)
		]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError _ -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
