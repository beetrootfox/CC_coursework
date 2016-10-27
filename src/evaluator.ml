open Ast
open String
open Hashtbl
open Printf

type value =
| Constant of int
| Id of string
| Command
| Comp of bool
| Nothing
| Reference of int
| Func of expression

let addr_gbl = ref 0

let fundefs = Hashtbl.create 100

let store = Hashtbl.create 100

let arrays = Hashtbl.create 100

let newref  () = addr_gbl := !addr_gbl + 1;
                !addr_gbl

let value_to_string = function
| Constant x -> ("Constant " ^ string_of_int x)
| Id s       -> ("Id " ^ s)
| Command    -> "Command"
| Comp b     -> ("Comp " ^ string_of_bool b)
| Nothing    -> "Nothing"
| Reference p -> ("Reference " ^ string_of_int p)
| Func exp -> ("Lambda " ^ exp_to_string exp)

let value_type_to_string = function
| Constant x -> "Constant of int"
| Id s       -> "Id of string"
| Command    -> "Command"
| Comp b     -> "Comp of bool"
| Nothing    -> "Nothing"
| Reference p -> "Reference of int"
| Func exp -> "Func of exp"

let err_string e v v' =
("Runtime error! Expression [ " ^ (exp_to_string e) ^  " ] is of type \'"^ (value_type_to_string v) ^
"\' but was expecting \'" ^ (value_type_to_string v') ^ "\'.")

let err_exit s = prerr_string (s ^ "\n"); exit(-1)

let rec eval_prog acc = function
| [] -> acc
| (fname, args, exp)::xs -> let (m, mexp) = acc in
if fname = m then err_exit "Program can only have one main function!" else
    if fname = "main" then (match args with
        | [] -> eval_prog (fname, exp) xs
        | x  -> err_exit "main() cannot have arguments!")
    else (Hashtbl.add fundefs fname (args,exp);
          eval_prog (m, mexp) xs)

let op_app_int op x y = match op with
| Plus -> Constant (x + y)
| Minus -> Constant (x - y)
| Times -> Constant (x * y)
| Divide -> Constant (x / y)
| Leq -> Comp (x < y)
| Geq -> Comp (x > y)
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| _ -> err_exit "Operators \'And\', \'OR\' are not supported by type 'Constant of int'"

let op_app_bool op x y = match op with
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| And -> Comp (x && y)
| Or -> Comp (x || y)
| _ -> err_exit "Operators \'Plus\', \'Minus\', \'Times\', \'Divide\', \'Leq\', \'Geq\' are not supported by type 'Comp of bool'"

let rec lookup x = function
| [] -> raise Not_found
| (n, l)::xs -> if n = x then l else lookup x xs

let rec fun_lookup x = function
| [] -> raise Not_found
| (fname, args, exp)::xs -> if x = fname then
                             (args, exp) else
                              fun_lookup x xs

let rec make_env x env acc = function
| ([],Empty) -> acc
| ([],_)     -> err_exit ("Function " ^ x ^ " is applied to too many arguments!")
| (_,Empty)  -> err_exit ("Function " ^ x ^ " is applied to too few arguments!")
| (a::args, Seq(e1, e2)) -> let v = eval_exp env e1 in
                            make_env x env ((a, v)::acc) (args, e2)
| (a::args, e)           -> let v = eval_exp env e in
                            make_env x env ((a, v)::acc) (args, Empty)

and eval_exp env = function
| Asg (e1, e2)    ->
    let id = eval_exp env e1 in
    (match id with
    | Reference l -> let v = eval_exp env e2 in
                                Hashtbl.replace store l v;
                                Command
    | Id s        -> err_exit (s ^ " is a Let-bound constant, but only variables can be assigned a value!")
    | _    -> err_exit (err_string e1 id (Id "")))
| Seq (e1, e2)    ->
    let _ = eval_exp env e1 in
    let v = eval_exp env e2 in
    v
| While (e1, e2)  ->
    let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                let e = Seq(e2, While(e1, e2)) in
                eval_exp env e
                else Command
    | _      -> err_exit (err_string e1 v (Comp true)))
| If (e1, e2, e3) ->
    let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                eval_exp env e2
                else
                eval_exp env e3
    | _      -> err_exit (err_string e1 v (Comp true)))
| Deref e         ->
    let id = eval_exp env e in
    (match id with
    | Reference l -> Hashtbl.find store l
    | Id x        -> err_exit (x ^ " is a Let-bound constant and cannot be dereferenced!")
    | _    -> err_exit (err_string e id (Id "")))
| Bin_Operator (op, e1, e2) ->
    let v1 = eval_exp env e1 in
    (match v1 with
    | Constant x ->
        let v2 = eval_exp env e2 in
        (match v2 with
        | Constant y -> op_app_int op x y
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | Comp x     ->
        (match op with
        | And -> if x then
                 let v2 = eval_exp env e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
                 else
                 Comp x
        | Or  -> if x then Comp x
                 else
                 let v2 = eval_exp env e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
        | _   ->
                 let v2 = eval_exp env e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true))))
    | Nothing ->
        let v2 = eval_exp env e2 in
        (match v2 with
        | Constant y -> Constant (-y)
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | _          -> err_exit
                    ("Binary operations are only supported by types \'Constant of int\', \'Comp of bool\' and \'Nothing\', expression [ " ^ exp_to_string e1 ^ " ] is of type \'" ^ value_type_to_string v1 ^ "\'!"))
| Unary_Operator (op, e) ->
        let v = eval_exp env e in
        (match v with
        | Comp b -> Comp (not b)
        | _      -> err_exit (err_string e v (Comp true)))
| Const n -> Constant n
| Identifier x -> let rv = (try lookup x env with
                                | Not_found -> Id x)
                  in rv
| Empty -> Nothing
| Printint e ->
        let v = eval_exp env e in
        Printf.printf ("%s\n") (value_to_string v); Command
| Let (x, e1, e2) ->
        let v = eval_exp env e1 in
        eval_exp ((x, v)::env) e2
| New (x, e1, e2) ->
        let v1 = eval_exp env e1 in
        let l  = newref () in
        Hashtbl.replace store l v1;
        let v2 = eval_exp ((x, Reference l)::env) e2 in
        Hashtbl.remove store l; v2
| Application (e1, e2) ->
        let f = eval_exp env e1 in
        (match f with
        | Id x -> let (args, exp) = (try Hashtbl.find fundefs x with
                                     | Not_found -> err_exit ("Function " ^ x ^ " was not defined!"))
                  in let env' = make_env x env env (args, e2) in
                  eval_exp env' exp
        | Func e -> (match e with
                     | Lambda (args, exp) -> let env' = make_env "lambda" env env (args, e2) in
                                             eval_exp env' exp
                     | _  -> err_exit (exp_to_string e ^ " must be a lambda function"))
        | _    -> err_exit (exp_to_string e1 ^ " is not a function!"))
| Lambda (args, e) as l -> Func l
| Array_make (x,e1,e2,e3) ->
        let index = eval_exp env e1 in
        (match index with
         | Constant n -> let first = eval_exp env e2 in
                         let ar = Array.make n first in
                         Hashtbl.replace arrays x ar;
                         let v2 = eval_exp env e3 in Hashtbl.remove arrays x; v2
        | _           -> err_exit "Array length must be an integer!")
| Array_set (e1, e2, e3) ->
        let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialized!"))
                   in let index = eval_exp env e2 in
                   (match index with
                    | Constant n -> let v = eval_exp env e3 in
                                    (match (Array.get ar 0, v) with
                                    | (Constant _, Constant _) -> (try Array.set ar n v with
                                                                   | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Id _, Id _)          ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Command, Command)    ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Comp _, Comp _)     ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Nothing, Nothing)   ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Reference _, Reference _) ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit "Array index out of bounds!"); Command
                                    | (Func _, Func _)  -> (try Array.set ar n v with
                                                           | Invalid_argument s-> err_exit "Array index out of bounds!"); Command
                                    | _ -> err_exit "Type mismatch!")
                    | _           -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!")
| Array_get (e1, e2) ->
        let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialised!"))
                       in let index = eval_exp env e2 in
                       (match index with
                        | Constant n -> (try Array.get ar n with
                                         | Invalid_argument s -> err_exit "Array index out of bounds!")
                        | _          -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!")
| _ -> err_exit "Functions, let bindings and IO are not implemented!"
;;

