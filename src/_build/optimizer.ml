open Ast
open String
open Str
open Hashtbl
open Printf
open Evaluator

type opt_value =
| Constant of int
| Id of string
| Command
| Comp of bool
| Nothing
| Reference of int
| Func of expression
| Opt of expression

let truth = Bin_Operator (Equal, Const 1, Const 1)

let lies  = Bin_Operator (Equal, Const 0, Const 1)

let contains s1 s2 =
try
let len = String.length s2 in
for i = 0 to String.length s1 - len do
if String.sub s1 i len = s2 then raise Exit
done;
false
with Exit -> true

let op_app_int_opt op x y = match op with
| Plus -> Constant (x + y)
| Minus -> Constant (x - y)
| Times -> Constant (x * y)
| Divide -> Constant (x / y)
| Leq -> Comp (x < y)
| Geq -> Comp (x > y)
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| _ -> err_exit "Operators \'And\', \'OR\' are not supported by type 'Constant of int'"

let op_app_bool_opt op x y = match op with
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| And -> Comp (x && y)
| Or -> Comp (x || y)
| _ -> err_exit "Operators \'Plus\', \'Minus\', \'Times\', \'Divide\', \'Leq\', \'Geq\' are not supported by type 'Comp of bool'"

let exp_of_val = function
| Constant n -> Const n
| Id s       -> Identifier s
| Comp b     -> if b then truth else lies
| Reference n -> Identifier "reference"
| Func exp    -> exp
| Opt exp     -> exp
| Nothing     -> Empty
| Command     -> Empty

let rec make_env_opt x env acc = function
| ([],[]) -> acc
| ([],_)     -> err_exit ("Function " ^ x ^ " is applied to too many arguments!")
| (_,[])  -> err_exit ("Function " ^ x ^ " is applied to too few arguments!")
| (a::args, e::es) -> let v = optimise_exp env e in
                      make_env_opt x env ((a, v)::acc) (args, es)


and unroll x i n exp acc = if n >= i then unroll x i (n-1) exp (Seq(Let (x, Const n, exp), acc))
                                     else acc

and optimise_exp env = function
| Asg (e1, e2)    -> Opt (Asg (optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val))
| Seq (e1, e2)    -> Opt (Seq (optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val))
| While (e1, e2)  -> Opt (While(optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val))
| For (x, e1, e2, e3) -> let start  = optimise_exp env e1 in
                         let finish = optimise_exp env e2 in
                         (match (start,finish) with
                          | (Constant n1, Constant n2) -> optimise_exp env (unroll x n1 (n2-1) e3 (Let (x, Const n2, e3)))
                          | _                          -> Opt (For (x, start |> exp_of_val, finish |> exp_of_val, optimise_exp env e3 |> exp_of_val)))
| If (e1, e2, e3) ->
    let v = optimise_exp env e1 in
    (match v with
    | Opt exp -> Opt (If(e1, optimise_exp env e2 |> exp_of_val, optimise_exp env e3 |> exp_of_val))
    | Comp c -> if c then
                optimise_exp env e2
                else
                optimise_exp env e3
    | _      -> err_exit "Optimiser error")
| Deref e -> Opt (Deref (optimise_exp env e |> exp_of_val))
| Bin_Operator (op, e1, e2) ->
    let v1 = optimise_exp env e1 in
    (match v1 with
    | Opt exp1    -> Opt (Bin_Operator (op, exp1, optimise_exp env e2 |> exp_of_val))
    | Constant x ->
        let v2 = optimise_exp env e2 in
        (match v2 with
        | Opt exp2   -> Opt (Bin_Operator (op, exp_of_val (Constant x), exp2))
        | Constant y -> op_app_int_opt op x y
        | _          -> err_exit "Optimiser error")
    | Comp x     ->
        (match op with
        | And -> if x then
                 let v2 = optimise_exp env e2 in
                (match v2 with
                    | Opt exp2 -> Opt (Bin_Operator (op, exp_of_val (Comp x), exp2))
                    | Comp y -> op_app_bool_opt op x y
                    | _      -> err_exit "Optimiser error")
                 else
                 Comp x
        | Or  -> if x then Comp x
                 else
                 let v2 = optimise_exp env e2 in
                (match v2 with
                    | Opt exp2 -> Opt (Bin_Operator (op, exp_of_val (Comp x), exp2))
                    | Comp y -> op_app_bool_opt op x y
                    | _      -> err_exit "Optimiser error")
        | _   ->
                 let v2 = optimise_exp env e2 in
                (match v2 with
                    | Opt exp2 -> Opt (Bin_Operator (op, exp_of_val (Comp x), exp2))
                    | Comp y -> op_app_bool_opt op x y
                    | _      -> err_exit "Optimiser error"))
    | Nothing ->
        let v2 = optimise_exp env e2 in
        (match v2 with
        | Opt exp2   -> Opt (Bin_Operator (op, exp_of_val Nothing, exp2))
        | Constant y -> Constant (-y)
        | _          -> err_exit "Optimiser error")
    | _          -> err_exit "Optimiser error")
| Unary_Operator (op, e) ->
        let v = optimise_exp env e in
        (match v with
        | Opt exp -> Opt (Unary_Operator (op, exp))
        | Comp b -> Comp (not b)
        | _      -> err_exit "Optimiser error")
| Const n -> Constant n
| Identifier x -> let rv = (try lookup x env with
                                | Not_found -> Id x)
                  in rv
| Empty -> Nothing
| Printint e ->
        let v = optimise_exp env e in
        Opt (Printint (exp_of_val v))
| Let (x, e1, e2) ->
        let v = optimise_exp env e1 in
        (match v with
        | Opt exp -> Opt (Let(x, exp, optimise_exp env e2 |> exp_of_val))
        | _       -> optimise_exp ((x, v)::env) e2)
| New (x, e1, e2) ->
        let v1 = optimise_exp env e1 in
        let v2 = optimise_exp env e2 in
        Opt (New(x, exp_of_val v1, exp_of_val v2))
| Application (e1, e2) -> (* write recursion check *)
        let f = optimise_exp env e1 in
        (match f with
        | Opt exp -> Opt (Application (exp, List.map (optimise_exp env) e2 |> List.map exp_of_val))
        | Id x -> let (args, exp) = (try Hashtbl.find fundefs x with
                                     | Not_found -> err_exit ("Function " ^ x ^ " was not defined!"))
                  in let s = exp_to_string exp in
                  if contains s x then Opt (Application (Identifier x, List.map (optimise_exp env) e2 |> List.map exp_of_val))
                  else
                  let env' = make_env_opt x env env (args, e2) in
                  let fv = optimise_exp env' exp in
                  (match fv with
                   | Opt _ -> Opt (Application (Identifier x, List.map (optimise_exp env) e2 |> List.map exp_of_val))
                   | _     -> fv)
        | Func e -> (match e with
                     | Lambda (args, exp) -> let env' = make_env_opt "lambda" env env (args, e2) in
                                                        let fv = optimise_exp env' exp in
                                                        (match fv with
                                                         | Opt _ -> Opt (Application (e, List.map (optimise_exp env) e2 |> List.map exp_of_val))
                                                         | _     -> fv)
                     | _  -> err_exit (exp_to_string e ^ " must be a lambda function"))
        | _    -> err_exit (exp_to_string e1 ^ " is not a function!"))
| Lambda (args, e) as l -> Func l
| Array_make (x,e1,e2,e3) -> Opt (Array_make(x, optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val, optimise_exp env e3 |> exp_of_val))
| Array_set (e1, e2, e3) -> Opt (Array_set(optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val, optimise_exp env e3 |> exp_of_val))
| Array_get (e1, e2) -> Opt (Array_get (optimise_exp env e1 |> exp_of_val, optimise_exp env e2 |> exp_of_val))
| Readint -> Opt (Readint)
;;
