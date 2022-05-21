open Pimp
open Printf

let pp_binop: binop -> string = function
  | Add -> "+"
  | Mul -> "*"
  | Div -> "/"
  | Lt  -> "<"
  | Le -> "<="
  | Sub -> "-"
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "=="

let pp_program prog out_channel =
  let print s = fprintf out_channel s in
  let margin = ref 0 in
  let sprint_margin () = String.make (2 * !margin) ' ' in
  
  let rec pp_expression: expression -> string = function
    | Cst(n) -> string_of_int n
    | Bool(b) -> if b then "true" else "false"
    | Var(x) -> x
    | Unop(Read, e) ->
       sprintf "*%s" (pp_expression e)
    | Unop(Alloc, e) ->
       sprintf "malloc(%s)" (pp_expression e)
    | Binop(op, e1, e2) ->
       sprintf "(%s%s%s)" (pp_expression e1) (pp_binop op) (pp_expression e2)
    | Call(FName f, args) ->
       sprintf "%s(%s)" f (pp_args args)
    | Call(FPointer e, args) ->
       sprintf "*%s(%s)" (pp_expression e) (pp_args args)
    | Addr(id) ->
       sprintf "&%s" id
    | Seq(s, e) ->
       incr margin;
       let s = pp_seq s in
       decr margin;
       sprintf "{\n%s%s} %s" s (sprint_margin()) (pp_expression e)
      
  and pp_args: expression list -> string = function
    | [] -> ""
    | [a] -> pp_expression a
    | a::args -> sprintf "%s,%s" (pp_expression a) (pp_args args)
               
  and pp_instruction: instruction -> string = function
    | Putchar(e) ->
      sprintf "putchar(%s);" (pp_expression e)
    | Set(x, e) ->
      sprintf "%s = %s;" x (pp_expression e)
    | If(c, s1, s2) ->
       let b = sprint_margin() in
       incr margin;
       let s1 = pp_seq s1 in
       let s2 = pp_seq s2 in
       decr margin;
       sprintf "if (%s) {\n%s%s} else {\n%s%s}"
         (pp_expression c) s1 b s2 b
    | While(c, s) ->
       incr margin;
       let s = pp_seq s in
       decr margin;
       sprintf "while (%s) {\n%s%s}" (pp_expression c) s (sprint_margin())
    | Return(e) ->
       sprintf "return(%s);" (pp_expression e)
    | Expr(e) ->
       sprintf "%s;" (pp_expression e)
    | Write(e1, e2) ->
       sprintf "*%s = %s;" (pp_expression e1) (pp_expression e2)

  and pp_seq = function
    | [] -> ""
    | i::seq -> sprintf "%s%s\n%s" (sprint_margin()) (pp_instruction i) (pp_seq seq)
  in

  let rec pp_params = function
    | [] -> ""
    | [x] -> x
    | x::params -> sprintf "%s,%s" x (pp_params params)
  in

  let pp_var x = sprintf "%svar %s;\n" (sprint_margin()) x in
  
  let rec pp_vars = function
    | [] -> ""
    | x::vars -> sprintf "%s%s" (pp_var x) (pp_vars vars)
  in
      
  let pp_function fdef =
    print "function %s(%s) {\n" fdef.name (pp_params fdef.params);
    incr margin;
    print "%s\n%s" (pp_vars fdef.locals) (pp_seq fdef.code);
    decr margin;
    print "}\n\n"
  in

  print "%s\n" (pp_vars prog.globals);
  List.iter pp_function prog.functions;