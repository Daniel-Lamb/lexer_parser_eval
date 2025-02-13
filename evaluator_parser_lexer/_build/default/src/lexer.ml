open Types

exception InvalidInputException of string

let tokenize input =
  let length = String.length input in

  let rec tok pos =
    if pos >= length then []
    else if Str.string_match (Str.regexp "[ \t\n]+") input pos then
      tok (pos + String.length (Str.matched_string input))

    else if Str.string_match (Str.regexp "(\\(-[0-9]+\\))") input pos then
      let value = Str.matched_string input in
      let stripped_value = String.sub value 1 (String.length value - 2) in
      Tok_Int (int_of_string stripped_value) :: tok (pos + String.length value)

    else if Str.string_match (Str.regexp "[0-9]+") input pos then
      let value = Str.matched_string input in
      Tok_Int (int_of_string value) :: tok (pos + String.length value)

    else if Str.string_match (Str.regexp "\\btrue\\b\\|\\bfalse\\b") input pos then
      let value = Str.matched_string input in
      Tok_Bool (bool_of_string value) :: tok (pos + String.length value)

    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
      let value = Str.matched_string input in
      let stripped_value = String.sub value 1 (String.length value - 2) in
      Tok_String stripped_value :: tok (pos + String.length value)

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi :: tok (pos + 2)

    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow :: tok (pos + 2)

    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual :: tok (pos + 2)

    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual :: tok (pos + 2)

    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual :: tok (pos + 2)

    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or :: tok (pos + 2)

    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And :: tok (pos + 2)

    else if Str.string_match (Str.regexp "\\+") input pos then
      Tok_Add :: tok (pos + 1)

    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub :: tok (pos + 1)

    else if Str.string_match (Str.regexp "\\*") input pos then
      Tok_Mult :: tok (pos + 1)

    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div :: tok (pos + 1)

    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat :: tok (pos + 1)

    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal :: tok (pos + 1)

    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater :: tok (pos + 1)

    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less :: tok (pos + 1)

    else if Str.string_match (Str.regexp "\\bnot\\b") input pos then
      Tok_Not :: tok (pos + 3)

    else if Str.string_match (Str.regexp "\\bif\\b") input pos then
      Tok_If :: tok (pos + 2)

    else if Str.string_match (Str.regexp "\\bthen\\b") input pos then
      Tok_Then :: tok (pos + 4)

    else if Str.string_match (Str.regexp "\\belse\\b") input pos then
      Tok_Else :: tok (pos + 4)

    else if Str.string_match (Str.regexp "\\blet\\b") input pos then
      Tok_Let :: tok (pos + 3)

    else if Str.string_match (Str.regexp "\\brec\\b") input pos then
      Tok_Rec :: tok (pos + 3)

    else if Str.string_match (Str.regexp "\\bin\\b") input pos then
      Tok_In :: tok (pos + 2)

    else if Str.string_match (Str.regexp "\\bdef\\b") input pos then
      Tok_Def :: tok (pos + 3)

    else if Str.string_match (Str.regexp "\\bfun\\b") input pos then
      Tok_Fun :: tok (pos + 3)

    else if Str.string_match (Str.regexp ";") input pos then
      Tok_Semi :: tok (pos + 1)

    else if Str.string_match (Str.regexp "{") input pos then
      Tok_LCurly :: tok (pos + 1)

    else if Str.string_match (Str.regexp "}") input pos then
      Tok_RCurly :: tok (pos + 1)

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen :: tok (pos + 1)

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen :: tok (pos + 1)

    else if Str.string_match (Str.regexp "\\.") input pos then
      Tok_Dot :: tok (pos + 1)

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let value = Str.matched_string input in
      Tok_ID value :: tok (pos + String.length value)

    else
      raise (InvalidInputException ("Invalid character at position " ^ string_of_int pos))
  in
  tok 0
