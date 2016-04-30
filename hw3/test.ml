
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
    ("if x+2 then x+2 else false", "dynamic type error");
    (* Function *)
    ("(function x -> x+2) 3", "5");
    ("(function false -> 3+6) false", "9");
    ("let f = function x -> if x > 0 then 1 else -1", "val f = <fun>");
    ("f (-2)", "-1");
    (* FunctionCall *)
    (* 1 *)
    ("let rec f num = if num > 0 then true else f (num+1)", "val f = <fun>");
    ("f 1", "true");
    ("f (-1)", "true");
    (* 2 *)
    ("let rec double i = i*2", "val double = <fun>");
    ("let rec twice f = function x -> f(f(x))", "val twice = <fun>");
    ("(twice double) 10", "40");
    (* 3 *)
    ("let x = 3", "val x = 3");
    ("let rec f y = x + y", "val f = <fun>");
    ("let x = 5", "val x = 5");
    ("x + (f 2)", "10");
    (* 4. special case to test global environment *)
    ("let x = 3", "val x = 3");
    ("let f = 4", "val f = 4");
    ("let rec f y = if y=3 then x else f(y-1)", "val f = <fun>");
    ("let x = 10", "val x = 10");
    ("f 5", "3");
    (* 5. recursion *)
    ("let rec sum n = if n=0 then 0 else n + sum(n-1)", "val sum = <fun>");
    ("sum 3", "6");
    (* 6. recursion *)
    ("let rec fact n = if n = 0 then 1 else n * fact (n - 1)", "val fact = <fun>");
    ("fact 3", "6");
    (* Tuple *)
    ("()", "()");
    ("(true)", "true");
    ("(2+4, 3, 4, true)", "(6, 3, 4, true)");
    ("let f = function (x, y, z) -> if x then y else z", "val f = <fun>");
    ("f (true, 2, 3)", "2");
    ("f (false, 2, 3)", "3");
    ("let q = function (x, y, z) -> if x then y+1 else z=4", "val q = <fun>");
    ("q (true, 2, 4)", "3");
    ("q (false, 2, 4)", "true");
    (* Data *)
    ("Leaf","Leaf");
    ("Node 1", "Node 1");
    ("Node (true,1,Leaf)", "Node (true, 1, Leaf)");
    ("let f = function Node x -> x + 1", "val f = <fun>");
    ("f (Node 2)", "3");
    ("f (Some 2)", "match failure");
    ("f (Node true)", "dynamic type error");
    ("let f = function Node (x, y) -> if x then y*10 else x", "val f = <fun>");
    ("f (Node (true, 2))", "20");
    ("f (Node (false, 2))", "false");
    ("f (Node (2, 2))", "dynamic type error");
    (* Match *)
    ("match (1,2) with (x,y) -> (2,30)", "(2, 30)");
    ("let z = 3", "val z = 3");
    ("match z with 4 -> false | 3 -> 3", "3");
    ("match 4 with x -> x*2", "8");
    ("match 1 with [] -> 1", "match failure");
    ("match 1 with 10 -> false | 1 -> true | _ -> 10 * z", "true");
    ("match 8 with 10 -> false | 1 -> true | _ -> 10 * z", "30");
    ("let bool = true", "val bool = true");
    ("match bool with false -> 0 | true -> 1", "1");
    ("let t = (1, true, z)", "val t = (1, true, 3)");
    ("match t with () -> false | (num, b, var) -> num", "1");
    ("match (1,2,3) with (2,2, 2) -> false | (2,2,3) -> false | (1,2,3) -> 69", "69");
(*    ("match ((), 2, (2, true, 10)) with () -> 0 | (e1, e2, e3) -> match e1 with () -> () | bool -> bool | [] -> false", "()")
   match e with ... e = IntVal / BoolVal / TupleVal / some moval.  *)

    (* from piazza *)
    ("let rec f ((x,y),(z,a)) = x+y+z+a", "val f = <fun>");
    ("f ((1,2),(4,5))", "12");
    ("let x = 3", "val x = 3");
    ("let f = 4", "val f = 4");
    ("let rec f x = if (2>0) then x else (f(x-1)) + (f(x-2))", "val f = <fun>");
    ("f 5", "5");
    ("let rec g g = g 0", "val g = <fun>");
    ("g (double)", "0");
    ("let rec f (Leaf(p,q,t,Node(x,y,z))) = function m -> if m > 0 then Leaf(p,q,t) else Node(x,y,z) ;; 
", "val f = <fun>");
    ("f ( Leaf(7,8,9,Node(10,15,16))) (-6)", "Node(10, 15, 16)");
    ("let rec f Leaf(p,q,t) =  function Node(x,y,z) -> function m -> if m > 0 then Leaf(p,q,t) else Node(x, y, z)", "val f = <fun>");
    ("f (Leaf(7,8,9)) (Node(1,2,3)) (6)", "Leaf(7, 8, 9)");

    (* from piazza - Daniel *)
                ("let double = function x -> x * 2", "val double = <fun>");
                ("double 6", "12");
                ("let two = 2", "val two = 2");
                ("let addTwo = function x -> x + two", "val addTwo = <fun>");
                ("addTwo 5", "7");
                ("let two = 3", "val two = 3");
                ("addTwo 5", "7");
                ("let add = function a -> function b -> a + b", "val add = <fun>");
                ("add 10 (-3)", "7");
                ("let p = (1, 2)", "val p = (1, 2)");
                ("let leaf = Leaf", "val leaf = Leaf");
                ("let node = Node(Leaf, 1, Leaf)", "val node = Node (Leaf, 1, Leaf)");
                ("let x = 34", "val x = 34");
                ("match x with 34 -> true | _ -> false", "true");
                ("match x with 35 -> true | _ -> false", "false");
                ("if true then 1 else 0", "1");
                ("match p with (a, b) -> a + b", "3");
                ("match p with (a, b, c) -> a + b + c", "match failure");
                ("match node with Node(l, v, r) -> (l, r)", "(Leaf, Leaf)");
                ("let iffPositive = function x -> if x > 0 then x else false", "val iffPositive = <fun>");
                ("iffPositive 3", "3");
                ("iffPositive (-3)", "false");
                ("let rec sumTree n = match n with Leaf -> 0 | Node(l, v, r) -> v + (sumTree l) + (sumTree r)", "val sumTree = <fun>");
                ("let a = Node(Leaf, 2, Leaf)", "val a = Node (Leaf, 2, Leaf)");
                ("let b = Node(Leaf, 3, Leaf)", "val b = Node (Leaf, 3, Leaf)");
                ("let c = Node(a, 11, b)", "val c = Node (Node (Leaf, 2, Leaf), 11, Node (Leaf, 3, Leaf))");
                ("let d = Node(Leaf, 5, Leaf)", "val d = Node (Leaf, 5, Leaf)");
                ("let root = Node(c, 100, d)", "val root = Node (Node (Node (Leaf, 2, Leaf), 11, Node (Leaf, 3, Leaf)), 100, Node (Leaf, 5, Leaf))");
                ("sumTree root", "121");
                ("let rec fib x = match x with 0 -> 0 | 1 -> 1 | n -> (fib (n-1)) + (fib (n-2))", "val fib = <fun>");
                ("fib 19", "4181");
                ("let rec fibIt a = function b -> function n -> if n > 0 then (fibIt b (a+b) (n-1)) else a", "val fibIt = <fun>");
                ("fibIt 0 1 19", "4181");




(*
  let x = 4 in (x*2) can be written in:
  match 4 with 
  x -> x*2
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
  
