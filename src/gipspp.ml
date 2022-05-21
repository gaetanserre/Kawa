open Gips
open Printf

let pp_unop: unop -> string = function
  | Subi n -> sprintf "%i- " n
  | Addi n -> sprintf "%i+ " n
  | ShiftL n -> sprintf "%i* " (n*2)
  | ShiftR n -> sprintf "(1/%d)* " (n*2)
  | Read -> "* "
  | Move   -> ""
   
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
  
let rec pp_args: string list -> string = function
  | [] -> ""
  | [a] -> a
  | a::args -> sprintf "%s,%s" a (pp_args args)

 let pp_func: func -> string = function
  | FName name
  | FPointer name -> name

let pp_program prog out_channel =
  let print s = fprintf out_channel s in
  
  let rec pp_instruction = function
    | Cst(d, n, l) ->
       print "%s <- %i        | %s" d n l
    | Unop(d, op, r, l) ->
       print "%s <- %s%s      | %s" d (pp_unop op) r l
    | Binop(d, op, r1, r2, l) ->
       print "%s <- %s %s %s  | %s" d r1 (pp_binop op) r2 l
    | GetLocal(d, k, l) ->
       print "%s <- local[%i] | %s" d k l
    | SetLocal(k, r, l) ->
       print "local[%i] <- %s | %s" k r l
    | GetGlobal(d, x, l) ->
       print "%s <- *%s       | %s" d x l
    | SetGlobal(x, r, l) ->
       print "*%s <- %s       | %s" x r l
    | GetParam(d, k, l) ->
       print "%s <- param[%i] | %s" d k l
    | Jump l ->
       print "jump            | %s" l
    | CJump(r, l1, l2) ->
       print "jump %s when %s | %s" l1 r l2
    | Call(f, l) ->
       print "call %s         | %s" (pp_func f) l
    | Syscall l ->
       print "syscall         | %s" l
    | Push(r, l) ->
       print "push(%s)        | %s" r l
    | Pop(r, l) ->
       print "%s <- pop()     | %s" r l
    | Return ->
       print "return"
    | Write (r1, r2, l) ->
      print "%s := %s         | %s" r1 r2 l
    | Addr (r, id, l) ->
      print "%s <- addr %s    | %s" r id l
  in

  let pp_var x = print "var %s;\n" x in
  let rec pp_vars = function
    | [] -> ()
    | [x] -> pp_var x; print "\n"
    | x::vars -> pp_var x; pp_vars vars
  in
  
  let pp_function fdef =
    print "function %s {\n" fdef.name;
    print "  entry: %s\n\n" fdef.entry;
    Hashtbl.iter (fun l i ->
        print "  %s:  " l;
        pp_instruction i;
        print "\n"
      ) fdef.code;
    print "}\n\n"
  in

  pp_vars prog.globals;
  List.iter pp_function prog.functions;
