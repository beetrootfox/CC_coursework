open Ast
open String
open Hashtbl

type value =
| Constant of int
| Id of string
| Command
| Comp of bool
| Nothing

let value_to_string = function
| Constant x -> String.concat "" ["Constant "; string_of_int x]
| Id s       -> String.concat "" ["Id "; s]
| Command    -> "Command"
| Comp b     -> String.concat "" ["Comp "; string_of_bool b]
| Nothing    -> "Nothing"

let value_type_to_string = function
| Constant x -> "Constant of int"
| Id s       -> "Id of string"
| Command    -> "Command"
| Comp b     -> "Comp of bool"
| Nothing    -> "Nothing"

let err_string e v v' =
("Runtime error! Expression [ " ^ (exp_to_string e) ^  " ] is of type \'"^ (value_type_to_string v) ^
"\' but was expecting \'" ^ (value_type_to_string v') ^ "\'.")

let err_exit s = prerr_string (s ^ "\n"); exit(-1)

let store = Hashtbl.create 100

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

let rec eval_exp = function
| Asg (e1, e2)    ->
    let id = eval_exp e1 in
    (match id with
    | Id s -> let v = eval_exp e2 in
              Hashtbl.replace store s v;
              Command
    | _    -> err_exit (err_string e1 id (Id "")))
| Seq (e1, e2)    ->
    let _ = eval_exp e1 in
    let v = eval_exp e2 in
    v
| While (e1, e2)  ->
    let v = eval_exp e1 in
    (match v with
    | Comp c -> if c then
                let e = Seq(e2, While(e1, e2)) in
                eval_exp e
                else Command
    | _      -> err_exit (err_string e1 v (Comp true)))
| If (e1, e2, e3) ->
    let v = eval_exp e1 in
    (match v with
    | Comp c -> if c then
                eval_exp e2
                else
                eval_exp e3
    | _      -> err_exit (err_string e1 v (Comp true)))
| Deref e         ->
    let id = eval_exp e in
    (match id with
    | Id s -> (try Hashtbl.find store s with
               | Not_found -> err_exit ("Variable " ^ s ^ " is undefined!"))
    | _    -> err_exit (err_string e id (Id "")))
| Bin_Operator (op, e1, e2) ->
    let v1 = eval_exp e1 in
    (match v1 with
    | Constant x ->
        let v2 = eval_exp e2 in
        (match v2 with
        | Constant y -> op_app_int op x y
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | Comp x     ->
        (match op with
        | And -> if x then
                 let v2 = eval_exp e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
                 else
                 Comp x
        | Or  -> if x then Comp x
                 else
                 let v2 = eval_exp e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
        | _   ->
                 let v2 = eval_exp e2 in
                (match v2 with
                    | Comp y -> op_app_bool op x y
                    | _      -> err_exit (err_string e2 v2 (Comp true))))
    | Nothing ->
        let v2 = eval_exp e2 in
        (match v2 with
        | Constant y -> Constant (-y)
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | _          -> err_exit
                    ("Binary operations are only supported by types \'Constant of int\', \'Comp of bool\' and \'Nothing\', expression [ " ^ exp_to_string e1 ^ " ] is of type \'" ^ value_type_to_string v1 ^ "\'!"))
| Unary_Operator (op, e) ->
        let v = eval_exp e in
        (match v with
        | Comp b -> Comp (not b)
        | _      -> err_exit (err_string e v (Comp true)))
| Const n -> Constant n
| Identifier x -> Id x
| Empty -> Nothing
| _ -> err_exit "Functions, let bindings and IO are not implemented!"
;;

