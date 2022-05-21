(**
MIMP : une variante de IMP incluant quelques opérations optimisées de MIPS.

La première étape de traduction, allant de IMP vers MIMP, a deux objectifs
- simplifier les expressions constantes
- introduire les instructions optimisées lorsque c'est possible
Le fichier principal pour cette traduction est imp2mimp.ml

On définit ici des "smart constructors" pour construire des opérations
arithmétiques en choisissant les opérateurs les plus adaptés.

Par rapport à IMP, on a aussi disparition des constantes booléennes, à
remplacer par des entiers (1 pour true et 0 pour false).
*)

(** 
Nouvelles opérations héritées de MIPS :
- addition d'une constante [Addi]
- décalage de bits [ShiftL]
Vous avez le droit d'étendre cette liste, tant que les nouvelles
opérations correspondent bien à des optimisations possibles en MIPS.
*)
type unop = Subi of int | Addi of int | ShiftL of int | ShiftR of int | Alloc | Read

(* Le reste des définitions est essentiellement identique *)
type binop = Sub | Add | Mul | Div | Lt | Le | Gt | Ge | Eq
                    
type expression =
| Cst   of int
| Var   of string
(* Seule nouveauté : une opération unaire, pour appliquer Addi et ShiftL *)
| Unop  of unop * expression
| Binop of binop * expression * expression
| Call  of func * expression list
| Seq   of sequence * expression
| Addr  of string

and func =
  | FName of string
  | FPointer of expression

and instruction =
| Putchar of expression
| Set     of string * expression
| If      of expression * sequence * sequence
| While   of expression * sequence
| Return  of expression
| Expr    of expression
| Write   of expression * expression
  
and sequence = instruction list

type function_def = {
name: string;
code: sequence;
params: string list;
locals: string list;
}

type program = {
functions: function_def list;
globals: string list;
}

(**
Smart constructors.

Un appel [mk_add e1 e2] construit une expression équivalente à

  Binop(Add, e1, e2)

mais tire parti lorsque c'est possible des formes de e1 et e2 pour
produire une expression plus simple.

Il faudra encore construire des fonctions équivalentes pour les 
autres opérations arithmétiques.
*)

(* Return 0 if not power of 2 *)
let get_log2 n =
  let rec aux n count =
    if n = 2 then count + 1
    else if n < 2 then 0
    else aux (n / 2) (count+1)
  in
  aux n 0

let mk_add_sub e1 e2 is_add =
match e1, e2 with
  | Cst n1, Cst n2 ->
    Cst (n1 + n2)
  | Cst 0, e | e , Cst 0 -> e
  | Cst n, e | e, Cst n ->
    Unop((if is_add then Addi n else Subi n), e)
  | e1', e2' ->
    Binop((if is_add then Add else Sub), e1', e2')

let mk_add e1 e2 =
  mk_add_sub e1 e2 true

let mk_sub e1 e2 =
  mk_add_sub e1 e2 false

let mk_mul e1 e2 =
  match e1, e2 with
    | Cst n1, Cst n2 ->
      Cst (n1 * n2)
    | Cst 0, e | e, Cst 0 -> Cst 0
    | Cst 1, e | e, Cst 1 -> e
    | Cst n, e | e, Cst n ->
    let log2 = get_log2 n in
    if log2 <> 0 then Unop(ShiftL log2, e)
    else Binop(Mul, e1, e2)
    | e1', e2' ->
      Binop(Mul, e1', e2')

let mk_div e1 e2 =
  match e1, e2 with
    | Cst n1, Cst n2 ->
      Cst (n1 / n2)
    | Cst 0, e -> Cst 0
    | e, Cst 1 -> e
    | e, Cst n ->
      let log2 = get_log2 n in
      if log2 <> 0 then Unop(ShiftR log2, e)
      else Binop(Div, e1, e2)
    | e1', e2' ->
      Binop(Div, e1', e2')
  

let mk_comp e1 e2 comp op =
match e1, e2 with
  | Cst n1, Cst n2 ->
    Cst (if comp n1 n2 then 1 else 0)
  | e1', e2' ->
    Binop(op, e1', e2')

let mk_lt e1 e2 =
  mk_comp e1 e2 (fun e1 e2 -> e1 < e2) Lt

let mk_le e1 e2 =
  mk_comp e1 e2 (fun e1 e2 -> e1 <= e2) Le

let mk_gt e1 e2 =
  mk_comp e1 e2 (fun e1 e2 -> e1 > e2) Gt

let mk_ge e1 e2 =
  mk_comp e1 e2 (fun e1 e2 -> e1 >= e2) Ge
  
let mk_eq e1 e2 =
  mk_comp e1 e2 (fun e1 e2 -> e1 = e2) Eq


          
