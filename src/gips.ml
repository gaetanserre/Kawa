(**
   Dernière représentation intermédiaire, toujours sous la forme d'un
   graphe mais où les instructions sont maintenant directement sous
   un format MIPS.
 *)

(* Les registres sont toujours représentés par des chaînes de caractères,
   mais ne désignent plus que les registres réels de MIPS. *)
type register = string
type label = string

type unop  = Subi of int | Addi of int | ShiftL of int | ShiftR of int | Move |  Read
type binop = Sub | Add | Mul | Div | Lt | Le | Gt | Ge | Eq

(** 
   De nombreuses instructions sont identiques à celles de VIPS, avec
   la contrainte supplémentaire de ne plus utiliser que des registres
   réels.
 *)
type instr =
  | Cst       of register * int * label
  | Unop      of register * unop * register * label
  | Binop     of register * binop * register * register * label
  | GetGlobal of register * string * label
  | SetGlobal of string * register * label
  | GetParam  of register * int * label
  | Jump      of label 
  | CJump     of register * label * label

  (* Première différence : instructions d'accès aux variables qui n'ont
     pas pu être affectée à un registre réel. Ces variables peuvent être
     des variables locales du programme d'origine, ou des variables
     temporaires créées lors de la traduction MIMP->VIPS. 
     Ces variables sont numérotées et placées dans le tableau d'activation, 
     aux adresses [$fp - 4*(k+2)]
   *)
  | GetLocal  of register * int * label
  | SetLocal  of int * register * label

  (* Deuxième différence : l'instruction d'appel de fonction ne fait plus
     que passer la main au code de la fonction appelée, en supposant que
     tout le travail préparatoire (par exemple : passage des arguments sur
     la pile) a déjà été fait. 
     De même, l'instruction de retour ne fait plus que rendre la main au
     contexte appelant, en supposant que le résultat a déjà été placé dans
     le bon registre et que le nettoyage du tableau d'activation est
     également déjà prévu. *)
  | Call      of func * label
  | Return

  (* Nouvelles instructions pour ajouter ou retirer des valeurs sur la
     pile, par exemple les arguments d'une fonction ou les autres éléments
     du tableau d'activation. 
       Push(r, next)  place sur la pile la valeur de [r]
       Pop(r, next)   retire le sommet de la pile (et place la valeur dans [r])
   *)
  | Push      of register * label
  | Pop       of register * label

  (* L'instruction syscall de MIPS *)
  | Syscall   of label

  | Write     of register * register * label

  | Addr      of register * string * label

and func =
  | FName of string
  | FPointer of register
  
type function_def = {
    name: string;
    code: (string, instr) Hashtbl.t;
    entry: label;
  }

type prog = {
    globals: string list;
    functions: function_def list;
  }

