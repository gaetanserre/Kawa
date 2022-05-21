(**
   Représentation intermédiaire, avec deux caractéristiques :
   - les expressions et instructions sous forme d'arbres de IMP et MIMP 
     sont remplacées par opérations atomiques travaillant directement sur 
     des registres (virtuels),
   - l'ensemble ne forme plus des séquences mais un graphe de flot de
     contrôle, c'est-à-dire un graphe dont les sommets sont des instructions
     et les arêtes donnent les successions entre instructions.

   Dans cette représentation on s'autorise à utiliser une infinité de registres
   appelés « registres virtuels ». Ce sera le rôle d'une phase ultérieure
   d'associer ces registres virtuels à des registres ou emplacements mémoires
   réels.

   Les registres virtuels sont utilisés pour :
   - les variables locales déclarées par une fonction,
   - les valeurs intermédiaires des calculs.

   Les registres virtuels NE SONT PAS utilisé pour :
   - les variables globales, qui restent associées à leur identifiant,
   - les paramètres de fonction, qui sont identifiés par leur numéro.
 *)

(* Les registres virtuels sont identifiés par des chaînes de caractères.
   La fonction [new_reg] produit de nouveaux registres à la demande. *)   
type register = string
let new_reg =
  let count = ref 0 in
  fun () -> incr count; Printf.sprintf "vreg_%i" !count

(* Chaque nœud du graphe est identifié par une étiquette, qui permet
   notamment d'identifier les successions. *)
type label = string

(* Les opérateurs restent, avec un seul ajout : une opération unaire [Move] 
   qui est en réalité l'identité, c'est-à-dire que son résultat est exactement
   la valeur de son opérande. On utilise cette instruction pour copier une 
   valeur d'un registre virtuel vers un autre. *)
type unop  = Subi of int | Addi of int | ShiftL of int | ShiftR of int | Move | Alloc | Read
type binop = Sub | Add | Mul | Div | Lt | Le | Gt | Ge | Eq

(** 
   Instructions.

   Toutes les instructions sont maintenant atomiques : leurs paramètres
   sont uniquement des registres ou des valeurs de base. En particulier,
   leur structure n'est plus récursive.

   Chaque instruction à l'exception de [Return] a pour dernier paramètre
   l'étiquette du nœud suivant, c'est-à-dire de l'instruction à exécuter
   ensuite.

   Les instructions calculant une valeur ont pour premier paramètre un
   registre virtuel qui contiendra la valeur calculée.
*)
type instr =
  (* Valeur immédiate
       Cst(r, n, next)
     place dans le registre [r] la valeur [n] *)
  | Cst       of register * int * label
               
  (* Opération unaire 
       Unop(d, op, r, next)
     applique l'opération unaire [op] à la valeur du registre [r] et 
     place le résultat dans le registre [d] *)
  | Unop      of register * unop * register * label
               
  (* Opération binaire
       Binop(d, op, r1, r2, next) 
     applique l'opération binaire [op] aux valeurs des registres [r1]
     et [r2] et place le résultat dans le registre [d] *)
  | Binop     of register * binop * register * register * label
               
  (* Lecture d'une variable globale 
       GetGlobal(d, x, next)
     place dans [r] la valeur de la variable globale [x] *)
  | GetGlobal of register * string * label

  (* Modification d'une variable globale 
       SetGlobal(x, r, next)
     remplace la valeur de la variable globale [x] par la valeur de [r] *)
  | SetGlobal of string * register * label

  (* Lecture d'un paramètre de fonction 
       GetParam(d, k, next)
     place dans [r] la valeur du [k]-ième paramètre de l'appel de fonction
     en cours d'exécution (la numérotation commençant à 0) *)
  | GetParam  of register * int * label
               
  (* Affichage
       Putchar(r, next)
     affiche le caractère donc le code ASCII est donné par [r] *)
  | Putchar   of register * label

  (* Saut inconditionnel 
       Jump(next)
     passe à l'instruction [next] sans rien faire d'autre *)
  | Jump      of label

  (* Saut conditionnel
       CJump(r, target, next)
     saute à l'instruction d'étiquette [target] si la valeur de [r] est
     différente de zéro (et sinon l'exécution continue avec [next]) *)
  | CJump     of register * label * label

  (* Appel de fonction 
       Call(d, f, rs, next)
     appelle la fonction [f] avec pour paramètres les valeurs des
     registres [rs], et place le résultat dans [d] *)
  | Call      of register * func * register list * label

  (* Fin d'un appel
       Return(r)
     termine l'appel de fonction en cours et renvoie la valeur de [r] *)
  | Return    of register

  | Write     of register * register * label

  | Addr      of register * string * label

and func =
  | FName of string
  | FPointer of register

(**
   Le code d'une fonction prend maintenant la forme d'un graphe.
 *)
type function_def = {
    name: string;
    (* Code : table associant étiquettes et instructions *)
    code: (string, instr) Hashtbl.t;
    (* Étiquette de la première instruction *)
    entry: label;
    (* Note : on ne mentionne plus les variables ni les paramètres *)
  }

type prog = {
    globals: string list;
    functions: function_def list;
  }