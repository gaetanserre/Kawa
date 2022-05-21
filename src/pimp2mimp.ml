(**
   Traduction de IMP vers MIMP.
   Deux objectifs
   - simplifier les expressions qui peuvent déjà être partiellement calculées,
   - sélectionner des opérateurs optimisés comme [Addi] lorsque c'est possible.
   La sélection repose sur des fonctions comme [mk_add] à définir dans le
   module MIMP.

   En dehors de ces simplifications et du codage des constantes booléennes par
   des entiers, la structure du programme reste la même.
 *)

open Mimp

let tr_unop u =
 match u with
  | Pimp.Alloc -> Alloc
  | Pimp.Read -> Read

let tr_func f isel_expr =
  match f with
   | Pimp.FName name -> FName name
   | Pimp.FPointer e -> FPointer (isel_expr e)

let mk_binop b =
  match b with
    | Pimp.Sub -> mk_sub
    | Pimp.Add -> mk_add
    | Pimp.Mul -> mk_mul
    | Pimp.Div -> mk_div
    | Pimp.Lt  -> mk_lt
    | Pimp.Le  -> mk_le
    | Pimp.Gt  -> mk_gt
    | Pimp.Ge  -> mk_ge
    | Pimp.Eq  -> mk_eq

let rec isel_expr: Pimp.expression -> Mimp.expression = function
  | Pimp.Cst n -> Cst n
  | Pimp.Bool b -> Cst (if b then 1 else 0)
  | Pimp.Var x -> Var x
  | Pimp.Unop (u, e) ->
    Unop (tr_unop u, isel_expr e)
  | Pimp.Binop(b, e1, e2) ->
     mk_binop b (isel_expr e1) (isel_expr e2)
  | Pimp.Call(f, el) ->
    Call(tr_func f isel_expr, List.map isel_expr el)
  | Pimp.Seq (s, e) ->
    Seq (isel_seq s, isel_expr e)
  | Pimp.Addr id -> Addr id

and isel_instr i =
  match i with
    | Pimp.Putchar e -> Putchar (isel_expr e)
    | Pimp.Set (s, e) -> Set (s, isel_expr e)
    | Pimp.If (e, sq1, sq2) ->
      If(isel_expr e, isel_seq sq1, isel_seq sq2)
    | Pimp.While (e, sq) ->
      While (isel_expr e, isel_seq sq)
    | Pimp.Return e -> Return (isel_expr e)
    | Pimp.Expr e -> Expr (isel_expr e)
    | Pimp.Write (e1, e2) -> Write (isel_expr e1, isel_expr e2)

and isel_seq s =
  List.map isel_instr s

let isel_fdef f = {
    name = Pimp.(f.name);
    code = isel_seq Pimp.(f.code);
    params = Pimp.(f.params);
    locals = Pimp.(f.locals);
  }

let isel_prog p = {
    functions = List.map isel_fdef Pimp.(p.functions);
    globals = Pimp.(p.globals);
  }
