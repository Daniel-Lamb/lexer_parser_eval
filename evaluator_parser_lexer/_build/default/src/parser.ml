open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input, got %s"
              (string_of_token tok) (string_of_token h)))

let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

let lookahead (toks : token list) =
  match toks with [] -> None | h :: _ -> Some h

let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  let (toks', expr) = parse_Expr toks in
  (toks', expr)

and parse_Expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks

and parse_LetExpr toks =
  let toks = match_token toks Tok_Let in
  let (toks, is_rec) =
    match lookahead toks with
    | Some Tok_Rec -> (match_token toks Tok_Rec, true)
    | _ -> (toks, false)
  in
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, expr1) = parse_expr toks in
      let toks = match_token toks Tok_In in
      let (toks, expr2) = parse_expr toks in
      (toks, Let (id, is_rec, expr1, expr2))
  | _ -> raise (InvalidInputException "Expected identifier in let expression")

and parse_FunctionExpr toks =
  let toks = match_token toks Tok_Fun in
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Arrow in
      let (toks, expr) = parse_expr toks in
      (toks, Fun (id, expr))
  | _ -> raise (InvalidInputException "Expected identifier in function expression")

and parse_IfExpr toks =
  let toks = match_token toks Tok_If in
  let (toks, expr1) = parse_expr toks in
  let toks = match_token toks Tok_Then in
  let (toks, expr2) = parse_expr toks in
  let toks = match_token toks Tok_Else in
  let (toks, expr3) = parse_expr toks in
  (toks, If (expr1, expr2, expr3))

and parse_OrExpr toks =
  let (toks, expr1) = parse_AndExpr toks in
  match lookahead toks with
  | Some Tok_Or ->
      let toks = match_token toks Tok_Or in
      let (toks, expr2) = parse_OrExpr toks in
      (toks, Binop (Or, expr1, expr2))
  | _ -> (toks, expr1)

and parse_AndExpr toks =
  let (toks, expr1) = parse_EqualityExpr toks in
  match lookahead toks with
  | Some Tok_And ->
      let toks = match_token toks Tok_And in
      let (toks, expr2) = parse_AndExpr toks in
      (toks, Binop (And, expr1, expr2))
  | _ -> (toks, expr1)

and parse_EqualityExpr toks =
  let (toks, expr1) = parse_RelationalExpr toks in
  match lookahead toks with
  | Some Tok_Equal ->
      let toks = match_token toks Tok_Equal in
      let (toks, expr2) = parse_EqualityExpr toks in
      (toks, Binop (Equal, expr1, expr2))
  | Some Tok_NotEqual ->
      let toks = match_token toks Tok_NotEqual in
      let (toks, expr2) = parse_EqualityExpr toks in
      (toks, Binop (NotEqual, expr1, expr2))
  | _ -> (toks, expr1)

and parse_RelationalExpr toks =
  let (toks, expr1) = parse_AdditiveExpr toks in
  match lookahead toks with
  | Some Tok_Less ->
      let toks = match_token toks Tok_Less in
      let (toks, expr2) = parse_RelationalExpr toks in
      (toks, Binop (Less, expr1, expr2))
  | Some Tok_Greater ->
      let toks = match_token toks Tok_Greater in
      let (toks, expr2) = parse_RelationalExpr toks in
      (toks, Binop (Greater, expr1, expr2))
  | Some Tok_LessEqual ->
      let toks = match_token toks Tok_LessEqual in
      let (toks, expr2) = parse_RelationalExpr toks in
      (toks, Binop (LessEqual, expr1, expr2))
  | Some Tok_GreaterEqual ->
      let toks = match_token toks Tok_GreaterEqual in
      let (toks, expr2) = parse_RelationalExpr toks in
      (toks, Binop (GreaterEqual, expr1, expr2))
  | _ -> (toks, expr1)

and parse_AdditiveExpr toks =
  let (toks, expr1) = parse_MultiplicativeExpr toks in
  match lookahead toks with
  | Some Tok_Add ->
      let toks = match_token toks Tok_Add in
      let (toks, expr2) = parse_AdditiveExpr toks in
      (toks, Binop (Add, expr1, expr2))
  | Some Tok_Sub ->
      let toks = match_token toks Tok_Sub in
      let (toks, expr2) = parse_AdditiveExpr toks in
      (toks, Binop (Sub, expr1, expr2))
  | _ -> (toks, expr1)

and parse_MultiplicativeExpr toks =
  let (toks, expr1) = parse_ConcatExpr toks in
  match lookahead toks with
  | Some Tok_Mult ->
      let toks = match_token toks Tok_Mult in
      let (toks, expr2) = parse_MultiplicativeExpr toks in
      (toks, Binop (Mult, expr1, expr2))
  | Some Tok_Div ->
      let toks = match_token toks Tok_Div in
      let (toks, expr2) = parse_MultiplicativeExpr toks in
      (toks, Binop (Div, expr1, expr2))
  | _ -> (toks, expr1)

and parse_ConcatExpr toks =
  let (toks, expr1) = parse_UnaryExpr toks in
  match lookahead toks with
  | Some Tok_Concat ->
      let toks = match_token toks Tok_Concat in
      let (toks, expr2) = parse_ConcatExpr toks in
      (toks, Binop (Concat, expr1, expr2))
  | _ -> (toks, expr1)

and parse_UnaryExpr toks =
  match lookahead toks with
  | Some Tok_Not ->
      let toks = match_token toks Tok_Not in
      let (toks, expr) = parse_UnaryExpr toks in
      (toks, Not expr)
  | _ -> parse_AppExpr toks

and parse_AppExpr toks =
  let (toks, expr1) = parse_SelectExpr toks in
  match lookahead toks with
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly) ->
      let (toks, expr2) = parse_AppExpr toks in
      (toks, App (expr1, expr2))
  | _ -> (toks, expr1)

and parse_SelectExpr toks =
  let (toks, expr1) = parse_PrimaryExpr toks in
  match lookahead toks with
  | Some Tok_Dot ->
      let toks = match_token toks Tok_Dot in
      (match lookahead toks with
      | Some (Tok_ID id) ->
          let toks = match_token toks (Tok_ID id) in
          (toks, Select (Lab id, expr1))
      | _ -> raise (InvalidInputException "Expected identifier after '.'"))
  | _ -> (toks, expr1)

and parse_PrimaryExpr toks =
  match lookahead toks with
  | Some (Tok_Int n) ->
      let toks = match_token toks (Tok_Int n) in
      (toks, Int n)
  | Some (Tok_Bool b) ->
      let toks = match_token toks (Tok_Bool b) in
      (toks, Bool b)
  | Some (Tok_String s) ->
      let toks = match_token toks (Tok_String s) in
      (toks, String s)
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      (toks, ID id)
  | Some Tok_LParen ->
      let toks = match_token toks Tok_LParen in
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_RParen in
      (toks, expr)
  | Some Tok_LCurly -> parse_RecordExpr toks
  | _ -> raise (InvalidInputException "Unexpected token in primary expression")

and parse_RecordExpr toks =
  let toks = match_token toks Tok_LCurly in
  match lookahead toks with
  | Some Tok_RCurly ->
      let toks = match_token toks Tok_RCurly in
      (toks, Record [])
  | _ ->
      let (toks, fields) = parse_RecordBodyExpr toks in
      let toks = match_token toks Tok_RCurly in
      (toks, Record fields)

and parse_RecordBodyExpr toks =
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, expr) = parse_expr toks in
      let toks, rest_fields =
        match lookahead toks with
        | Some Tok_Semi ->
            let toks = match_token toks Tok_Semi in
            let (toks, rest) = parse_RecordBodyExpr toks in
            (toks, rest)
        | _ -> (toks, [])
      in
      (toks, (Lab id, expr) :: rest_fields)
  | _ -> raise (InvalidInputException "Expected identifier in record field")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def -> parse_DefMutop toks
  | Some Tok_DoubleSemi ->
      let toks = match_token toks Tok_DoubleSemi in
      (toks, NoOp)
  | _ ->
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_DoubleSemi in
      (toks, Expr expr)

and parse_DefMutop toks =
  let toks = match_token toks Tok_Def in
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_DoubleSemi in
      (toks, Def (id, expr))
  | _ -> raise (InvalidInputException "Expected identifier in definition")
