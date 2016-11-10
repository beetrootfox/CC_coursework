open Ast
open String
open Hashtbl
open Printf
open Evaluator

let ram = ((Hashtbl.create 1000) : ((int,value) Hashtbl.t))

let acc = ref Nothing

let stack_base = ref 500
let heap_base = ref 499

let rr = 500
let _ = Hashtbl.replace ram rr (Constant 99)
let wr = 499

let si = 500


let stackbond = 1000
let heapbond = 0

let inc_stp () = if !stack_base = 1000 then err_exit "stack overflow"
                                       else stack_base := !stack_base + 1;
                                            !stack_base

let inc_hpp () = if !heap_base = 0 then err_exit "not enough memory to perform allocation"
                                   else heap_base := !heap_base - 1;
                                        !heap_base

let ins_str_of_operator = function
| Plus -> "add"
| Minus -> "sub"
| Times -> "mul"
| Divide -> "div"
| Leq -> "lt"
| Geq -> "gt"
| Equal -> "eq"
| Noteq -> "neq"
| And -> "and"
| Or -> "or"

(*module type InstructionApplier = sig
val op_int_ins : bin_opcode -> int -> int -> unit
val op_bool_ins : bin_opcode -> int -> int -> unit
val unary_op_ins : unary_opcode -> int -> unit
val st_ins : int -> unit
val ldc_ins : value -> unit
val ldr_ins : int -> unit
val mv_ins : int -> int -> unit
end*)

module type InstructionApplier = sig
val op_ins : bin_opcode -> int -> int -> unit
val unary_op_ins : unary_opcode -> int -> unit
val st_ins : int -> unit
val ldc_ins : value -> unit
val ldr_ins : int -> unit
val mv_ins : int -> int -> unit
end

(*let codegen_op op addr1 addr2 =
    (ins_str_of_operator op)
    ^ " r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code
let codegen_st addr = "st r" ^ (string_of_int addr)
                       ^ "\n" |> Buffer.add_string code
let codegen_ldc n = "ld " ^ (string_of_int n)
                     ^ "\n" |> Buffer.add_string code

let op_int_ins op addr1 addr2 = acc := (match op with
| Plus -> Constant (x + y)
| Minus -> Constant (x - y)
| Times -> Constant (x * y)
| Divide -> Constant (x / y)
| Leq -> Comp (x < y)
| Geq -> Comp (x > y)
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| _ -> err_exit "Operators \'And\', \'OR\' are not supported by type 'Constant of int'")

let op_bool_ins op addr1 addr2 = acc := (match op with
| Equal -> Comp (x = y)
| Noteq -> Comp (x != y)
| And -> Comp (x && y)
| Or -> Comp (x || y)
| _ -> err_exit "Operators \'Plus\', \'Minus\', \'Times\', \'Divide\', \'Leq\', \'Geq\' are not supported by type 'Comp of bool'")

let st addr = Hashtbl.replace ram addr !acc
let ldc n = acc := n

let rec make_env x env acc = function
| ([],[]) -> acc
| ([],_)  -> err_exit ("Function " ^ x ^ " is applied to too many arguments!")
| (_,[])  -> err_exit ("Function " ^ x ^ " is applied to too few arguments!")
| (a::args, e::es) -> let v = eval_exp env e in
                            make_env x env ((a, v)::acc) (args, es)*)

module Interpreter (INA : InstructionApplier) = struct

let rec interpret symt = function
| Asg (e1, e2)    ->
    let haddr = interpret symt e1 in
    if haddr < si then let addr = interpret symt e2 in
    INA.ldr_ins addr;
    INA.st_ins haddr;
    addr
    else err_exit ("could not perfor assignment on r" ^ (string_of_int haddr))
| Seq (e1, e2)    ->
    let _ = interpret symt e1 in
    let addr = interpret symt e2 in
    addr
| While (e1, e2)  -> !stack_base
    (*let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                let e = Seq(e2, While(e1, e2)) in
                eval_exp env e
                else Command
    | _      -> err_exit (err_string e1 v (Comp true)))*)
| For (x, e1, e2, e3) -> !stack_base
    (*let v1 = eval_exp env e1 in
    let v2 = eval_exp env e2 in
    (match (v1,v2) with
    | (Constant n1, Constant n2) -> if n1 <= n2 then
                                    let e = Seq(e3, For(x, Const(n1 + 1), Const n2, e3)) in
                                    eval_exp ((x,v1)::env) e
                                    else Command
    | _                          -> err_exit "For loop boundaries must be integers!")*)
| If (e1, e2, e3) -> !stack_base
    (*let v = eval_exp env e1 in
    (match v with
    | Comp c -> if c then
                eval_exp env e2
                else
                eval_exp env e3
    | _      -> err_exit (err_string e1 v (Comp true)))*)
| Deref e         ->
    let haddr = interpret symt e in
    if haddr < si then let addr = inc_stp () in
                    INA.mv_ins haddr addr;
                    addr
    else err_exit ("could not dereference address" ^ (string_of_int haddr))
| Bin_Operator (op, e1, e2) ->
    let addr1 = interpret symt e1 in
    let addr2 = interpret symt e2 in
    INA.op_ins op addr1 addr2;
    stack_base := addr1;
    INA.st_ins addr1;
    addr1
(*| Bin_Operator (op, e1, e2) ->
    let addr1 = interpret symt e1 in
    let v1 = Hashtbl.find ram addr1 in
    (match v1 with
    | Constant x ->
        let addr2 = interpret symt e2 in
        let v2 = Hashtbl.find ram addr2 in
        (match v2 with
        | Constant y -> INA.op_int_ins op addr1 addr2;
                        stack_base := addr1;
                        INA.st_ins addr1;
                        addr1
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | Comp x     ->
        (match op with
        | And -> if x then
                 let addr2 = interpret symt e2 in
                 let v2 = Hashtbl.find ram addr2 in
                (match v2 with
                    | Comp y -> INA.op_bool_ins op addr1 addr2;
                                stack_base := addr1;
                                INA.st_ins addr1;
                                addr1
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
                 else
                 (stack_base := addr1;
                 addr1)
        | Or  -> if x then
                 (stack_base := addr1; addr1)
                 else
                 let addr2 = interpret symt e2 in
                 let v2 = Hashtbl.find ram addr2 in
                (match v2 with
                    | Comp y -> INA.op_bool_ins op addr1 addr2;
                                stack_base := addr1;
                                INA.st_ins addr1;
                                addr1
                    | _      -> err_exit (err_string e2 v2 (Comp true)))
        | _   ->
                 let addr2 = interpret symt e2 in
                 let v2 = Hashtbl.find ram addr2 in
                (match v2 with
                    | Comp y -> INA.op_bool_ins op addr1 addr2;
                                stack_base := addr1;
                                INA.st_ins addr1;
                                addr1
                    | _      -> err_exit (err_string e2 v2 (Comp true))))
    | Nothing ->
        let addr2 = interpret symt e2 in
        let v2 = Hashtbl.find ram addr2 in
        (match v2 with
        | Constant y -> INA.ldc_ins (Constant (-y));
                        stack_base := addr1;
                        INA.st_ins addr1;
                        addr1
        | _          -> err_exit (err_string e2 v2 (Constant 1)))
    | _          -> err_exit
                    ("Binary operations are only supported by types \'Constant of int\', \'Comp of bool\' and \'Nothing\', expression [ " ^ exp_to_string e1 ^ " ] is of type \'" ^ value_type_to_string v1 ^ "\'!"))*)
(*| Unary_Operator (op, e) ->
        let addr = interpret symt e in
        let v = Hashtbl.find ram addr in
        (match v with
        | Comp b -> INA.unary_op_ins op addr;
                    stack_base := addr; (*check if this is redundant*)
                    INA.st_ins addr;
                    addr
        | _      -> err_exit (err_string e v (Comp true)))*)
| Unary_Operator (op, e) ->
    let addr = interpret symt e in
    INA.unary_op_ins op addr;
    stack_base := addr;
    INA.st_ins addr;
    addr
| Const n ->
        let addr = inc_stp () in
        INA.ldc_ins (Constant n);
        INA.st_ins addr;
        addr
| Identifier x ->
        let addr = (try lookup x symt with
                                | Not_found ->
                                    let addr' = inc_stp () in
                                    INA.ldc_ins (Id x);
                                    INA.st_ins addr';
                                    addr') in
        if addr < si then addr
        else (
                let addr' = inc_stp () in
                INA.mv_ins addr addr'; addr'
             )
| Empty -> !stack_base
| Printint e -> let sp = !stack_base in
                let addr = interpret symt e in
                INA.mv_ins addr wr;
                sp
| Let (x, e1, e2) ->
        let addr1 = interpret symt e1 in
        let addr2 = interpret ((x, addr1) :: symt) e2 in
            if addr1 < si then addr2 else 
                   ( INA.mv_ins addr2 addr1;
                    stack_base := addr1;
                    addr1)
| New (x, e1, e2) ->
        let addr1 = interpret symt e1 in
        let haddr = inc_hpp () in
        INA.mv_ins addr1 haddr;
        let addr2 = interpret ((x, haddr)::symt) e2 in
        heap_base := !heap_base - 1;
        INA.mv_ins addr2 addr1;
        stack_base := addr1;
        addr1
| Application (e1, e2) -> !stack_base
        (*let f = eval_exp env e1 in
        (match f with
        | Id x -> let (args, exp) = (try Hashtbl.find fundefs x with
                                     | Not_found -> err_exit ("Function " ^ x ^ " was not defined!"))
                  in let env' = make_env x env env (args, e2) in
                  eval_exp env' exp
        | Func e -> (match e with
                     | Lambda (args, exp) -> let env' = make_env "lambda" env env (args, e2) in
                                             eval_exp env' exp
                     | _  -> err_exit (exp_to_string e ^ " must be a lambda function"))
        | _    -> err_exit (exp_to_string e1 ^ " is not a function!"))*)
| Lambda (args, e) as l -> !stack_base (*Func l*)
| Array_make (x,e1,e2,e3) -> !stack_base
        (*let index = eval_exp env e1 in
        (match index with
         | Constant n -> let first = eval_exp env e2 in
                         let ar = Array.make n first in
                         Hashtbl.replace arrays x ar;
                         let v2 = eval_exp env e3 in Hashtbl.remove arrays x; v2
         | _           -> err_exit "Array length must be an integer!")*)
| Array_set (e1, e2, e3) -> !stack_base
        (*let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialized!"))
                   in let index = eval_exp env e2 in
                   (match index with
                    | Constant n -> let v = eval_exp env e3 in
                                    (match (Array.get ar 0, v) with
                                    | (Constant _, Constant _) -> (try Array.set ar n v with
                                                                   | Invalid_argument s -> err_exit s); Command
                                    | (Id _, Id _)          ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Command, Command)    ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Comp _, Comp _)     ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Nothing, Nothing)   ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Reference _, Reference _) ->(try Array.set ar n v with
                                                                    | Invalid_argument s -> err_exit s); Command
                                    | (Func _, Func _)  -> (try Array.set ar n v with
                                                           | Invalid_argument s-> err_exit s); Command
                                    | _ -> err_exit "Type mismatch!")
                    | _           -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!") *)
| Array_get (e1, e2) -> !stack_base
       (* let name = eval_exp env e1 in
        (match name with
         | Id x -> let ar = (try Hashtbl.find arrays x with
                             | Not_found -> err_exit ("Array " ^ x ^ " was not initialised!"))
                       in let index = eval_exp env e2 in
                       (match index with
                        | Constant n -> (try Array.get ar n with
                                         | Invalid_argument s -> err_exit "Array index out of bounds!")
                        | _          -> err_exit "Array index must be an int!")
         | _    -> err_exit "Array name must be an identifier!")*)
| Readint -> let addr = inc_stp () in
             INA.mv_ins rr addr;
             addr
end
;;
