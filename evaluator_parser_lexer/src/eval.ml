
open Utils

exception TypeError of string
exception DeclareError of string
exception SelectError of string
exception DivByZeroError

open Types

(* Extend env with new var binding *)
let extend env x v = (x, ref v) :: env

(* Lookup var in env, return val *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Extend env with temp var initialized to 0 *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Update existing var*)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Recursively eval expr*)
let rec eval_expr env e =
  match e with
  | Int n -> Int n  (* Return int literals*)
  | Bool b -> Bool b  (* Return boolean literals*)
  | String s -> String s  (* Return string literals*)
  | ID x -> lookup env x  (* Retrieve variable value from environment *)
  | Not e1 ->
      let v = eval_expr env e1 in
      (match v with
      | Bool b -> Bool (not b)  (* negation *)
      | _ -> raise (TypeError "Expected type bool"))
  | Binop (op, e1, e2) ->  (* binary ops eval *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match (op, v1, v2) with
      | (Add, Int n1, Int n2) -> Int (n1 + n2)  (* int add *)
      | (Sub, Int n1, Int n2) -> Int (n1 - n2)  (* subtract *)
      | (Mult, Int n1, Int n2) -> Int (n1 * n2)  (* mult *)
      | (Div, Int n1, Int n2) ->  (* zero check *)
          if n2 = 0 then raise DivByZeroError else Int (n1 / n2)
      | (Concat, String s1, String s2) -> String (s1 ^ s2)  (* concatenation *)
      | (Greater, Int n1, Int n2) -> Bool (n1 > n2)  (* Comparison ops *)
      | (Less, Int n1, Int n2) -> Bool (n1 < n2)
      | (GreaterEqual, Int n1, Int n2) -> Bool (n1 >= n2)
      | (LessEqual, Int n1, Int n2) -> Bool (n1 <= n2)
      | (Equal, Int n1, Int n2) -> Bool (n1 = n2)
      | (Equal, Bool b1, Bool b2) -> Bool (b1 = b2)
      | (Equal, String s1, String s2) -> Bool (s1 = s2)
      | (NotEqual, Int n1, Int n2) -> Bool (n1 <> n2)
      | (NotEqual, Bool b1, Bool b2) -> Bool (b1 <> b2)
      | (NotEqual, String s1, String s2) -> Bool (s1 <> s2)
      | (Or, Bool b1, Bool b2) -> Bool (b1 || b2)  (* OR *)
      | (And, Bool b1, Bool b2) -> Bool (b1 && b2)  (* AND *)
      | _ -> raise (TypeError "Invalid operands to binary operator"))
  | If (e1, e2, e3) ->  (* Evals cond exprs *)
      let v1 = eval_expr env e1 in
      (match v1 with
      | Bool true -> eval_expr env e2
      | Bool false -> eval_expr env e3
      | _ -> raise (TypeError "Guard of if expression must be a boolean"))
  | Let (x, is_rec, e1, e2) ->  (* Let bindings with recursion *)
      if is_rec then
        let env' = extend_tmp env x in
        let v1 = eval_expr env' e1 in
        update env' x v1;
        eval_expr env' e2
      else
        let v1 = eval_expr env e1 in
        let env' = extend env x v1 in
        eval_expr env' e2
  | Fun (x, e1) -> Closure (env, x, e1)  (* Function definition *)
  | App (e1, e2) ->  (* Function application *)
      let v1 = eval_expr env e1 in
      (match v1 with
      | Closure (env', x, body) ->
          let v2 = eval_expr env e2 in
          let env'' = extend env' x v2 in
          eval_expr env'' body
      | _ -> raise (TypeError "First arg != function"))
  | Record fields ->  (* Record creation *)
      let rec eval_fields flist =
        match flist with
        | [] -> []
        | (Lab l, expr) :: rest ->
            let v = eval_expr env expr in
            (Lab l, v) :: eval_fields rest
      in
      Record (eval_fields fields)
  | Select (Lab l, e1) ->  (* field selection *)
      let v = eval_expr env e1 in
      (match v with
      | Record fields -> (
          try List.assoc (Lab l) fields
          with Not_found -> raise (SelectError ("Label " ^ l ^ " not found")))
      | _ -> raise (TypeError "Not record"))
  | Closure _ -> e  (* Returns the closure as is *)




(* Part 2: Evaluating mutop directive *)

let eval_mutop env m =
  match m with
  | Def (x, e) ->  (* Defines new var *)
      let env' = extend_tmp env x in
      let v = eval_expr env' e in
      update env' x v;
      (env', Some v)
  | Expr e ->  (* Evaluates expr*)
      let v = eval_expr env e in
      (env, Some v)
  | NoOp -> (env, None)  (* No op case *)
