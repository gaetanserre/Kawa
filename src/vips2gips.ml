(**
   Traduction entre deux représentations intermédiaires.

   Départ : VIPS
     graphe d'instructions élémentaires
     infinité de registres virtuels pour variables et valeurs temporaires

   Arrivée : GIPS
     graphe d'instructions élémentaires
     seulement registres physiques et emplacement de pile

   Le cœur de la traduction consiste à effectuer l'allocation de 
   registres. En passant, il faut ajuster certaines des opérations, pour
   ajouter des instructions de lecture ou d'écriture sur la pile lorsque
   l'on manipule des valeurs qui ont été affectées à cet endroit.

   En outre, on va profiter de cette traduction pour expliciter les
   conventions d'appel des fonctions : passer les paramètres, sauvegarder
   et restaurer les registres qui doivent l'être, allouer et désallouer 
   le tableau d'activation, renvoyer le résultat...
 *)

open Gips
open Register_allocation

(**
   Fonctions auxiliaires : traduction à l'identique des opérateurs
 *)
let tr_unop = function
  | Vips.Subi n -> Subi n
  | Vips.Addi n -> Addi n
  | Vips.Move   -> Move
  | Vips.ShiftL n -> ShiftL n
  | Vips.ShiftR n -> ShiftR n
  | Vips.Alloc -> assert false
  | Vips.Read -> Read
  
let tr_binop = function
  | Vips.Sub -> Sub
  | Vips.Add -> Add
  | Vips.Mul -> Mul
  | Vips.Div -> Div
  | Vips.Lt  -> Lt
  | Vips.Le -> Le
  | Vips.Gt -> Gt
  | Vips.Ge -> Ge
  | Vips.Eq -> Eq

(**
   Fonction principale : traduction d'une fonction VIPS en fonction GIPS. 
   Note : l'allocation de registre est faite à l'échelle d'une fonction.
 *)
let translate_fdef fdef =
  (* Initialisation d'une nouvelle table de hachage pour représenter le
     graphe de code de la fonction GIPS produite. *)
  let code = Hashtbl.create 32 in
  (* On utilise comme précédemment deux fonctions auxiliaires de création
     d'une nouvelle étiquette et d'ajout d'un nouveau nœud au graphe.
     Note : comme on conserva les étiquettes déjà utilisées dans la
     version VIPS, cette nouvelle fonction [new_label] construit des noms
     de forme différente.
   *)
  let new_label =
    let f = Vips.(fdef.name) in
    let count = ref 0 in
    fun () -> incr count; Printf.sprintf "%s_g_%i" f !count
  in
  let add_instr i =
    let l = new_label () in
    Hashtbl.add code l i;
    l
  in

  (* On récupère l'allocation des registres et le nombre d'emplacement de 
     pile utilisés. *)
  let alloc, nb_locals = allocate_function fdef in

  (** Quelques registres physiques MIPS que l'on manipulera à la main. *)
  (* Registres de travail pour manipuler les valeurs venant de la pile *)
  let tmp1 = "$t0" in
  let tmp2 = "$t1" in
  (* Registres utilisés par [syscall] *)
  let arg  = "$a0" in
  let scod = "$v0" in
  (* Registre pour le résultat d'un appel de fonction *)
  let ret  = "$v0" in
  (* Sommet de pile *)
  let sp   = "$sp" in
  (* Adresse de base du tableau d'activation d'un appel *)
  let fp   = "$fp" in
  (* Adresse de retour *)
  let ra   = "$ra" in

  (**
     Traduction d'une instruction VIPS en une ou plusieurs 
     instructions GIPS.

     Pour éviter d'avoir à renommer tous les champs [next] des
     instructions, on va garder les étiquettes déjà présentes.
     En revanche, on va créer à la volée de nouveaux nœuds et de
     nouvelles étiquettes lorsqu'une instruction VIPS est traduite
     par plusieurs instructions GIPS.

     Si une instruction VIPS [i] doit être traduite par une séquence
     d'instructions GIPS [i1; i2; ...; iN], on va créer de nouveaux
     nœuds et de nouvelles étiquettes pour les instructions [i2] à [iN],
     et garder pour [i1] l'étiquette qui était celle de [i].

     Dans ce cas, [tr_instr] renvoie l'instruction GIPS [i1] après avoir
     ajouté au graphe les instructions [iN] à [i2]. Dans tous les cas,
     [tr_instr] ne renvoie donc qu'une unique instruction GIPS.
   *)
  let tr_instr i =
  let tr_virtual_register register f p1 p2 =
    match Hashtbl.find alloc register with
      | Register r' -> f r' p1 p2
      | Stacked k ->
        let l = add_instr (f tmp1 p1 p2) in
        GetLocal (tmp1, k, l)
  in

  let tr_virtual_binop r r1 r2 b next =
    match Hashtbl.find alloc r1, Hashtbl.find alloc r2 with
      | Register r1', Register r2' ->
        Binop (r, b, r1', r2', next)
      | Stacked k1, Register r2' ->
        let l = add_instr(Binop(r, b, tmp1, r2', next)) in
        GetLocal (tmp1, k1, l)
      | Register r1', Stacked k2 ->
        let l = add_instr(Binop(r, b, r1', tmp1, next)) in
        GetLocal (tmp1, k2, l)
      | Stacked k1, Stacked k2 ->
        let l = add_instr (Binop(r, b, tmp1, tmp2, next)) in
        let l = add_instr (GetLocal(tmp2, k2, l)) in
        GetLocal(tmp1, k1, l)
  in

   match i with

    (* Cas du chargement d'une constante *)
    | Vips.Cst(r, n, next) ->
       (* On consulte l'allocation du registre de destination [r] *)
       begin match Hashtbl.find alloc r with
       (* S'il s'agit d'un registre physique, on utilise directement
          l'instruction GIPS correspondante *)
       | Register r' -> Cst(r', n, next)
       (* Sinon on veut écrire la valeur [n] à une certaine adresse
          de la pile. Pour cela il faut d'abord charger cette valeur 
          dans un registre de travail, puis la transfèrer sur la pile. 
          Donc : on crée un nouveau nœud pour la deuxième opération
          (instruction d'écriture [SetLocal]), puis on renvoie
          l'instruction correspondant à la première étape. *)
       | Stacked k ->
          let l = add_instr (SetLocal(k, tmp1, next)) in
          Cst(tmp1, n, l)
       end

    (* Cas d'un opérateur unaire
       On va produire entre une et trois opérations selon les allocations
       de registre de destination et de l'opérande. Ci-dessous, un
       fragment de code où tous les cas à traiter sont détaillés. On peut
       factoriser ce programme en définissant les bonnes fonctions
       auxiliaires. *)

    | Vips.Unop (r1, Alloc, r2, next) ->
      begin match Hashtbl.find alloc r1 with
       | Register r1' ->
          let l = add_instr (Unop (r1', Move, scod, next)) in
          let l = add_instr (Syscall l) in
          let l = add_instr (Cst(scod, 9, l)) in
          begin match Hashtbl.find alloc r2 with
            | Register r2' ->
              Unop (arg, Move, r2', l)
            | Stacked k2 ->
              GetLocal(arg, k2, l)
          end
       | Stacked k1 ->
          let l = add_instr (SetLocal(k1, scod, next)) in
          let l = add_instr (Syscall l) in
          let l = add_instr (Cst(scod, 9, l)) in
          begin match Hashtbl.find alloc r2 with
            | Register r2' ->
              Unop (arg, Move, r2', l)
            | Stacked k2 ->
              GetLocal(arg, k2, l)
          end
       end
    
    | Vips.Unop(r1, op, r2, next) ->
       begin match Hashtbl.find alloc r1 with
       | Register r1' ->
          begin match Hashtbl.find alloc r2 with
            | Register r2' -> Unop(r1', tr_unop op, r2', next)
            | Stacked k2 ->
              let l = add_instr (Unop(r1', tr_unop op, tmp1, next)) in
              GetLocal(tmp1, k2, l)
          end
       | Stacked k1 ->
          begin match Hashtbl.find alloc r2 with
            | Register r2' ->
              let l = add_instr (SetLocal(k1, tmp1, next)) in
              Unop (tmp1, tr_unop op, r2', l)
            | Stacked k2 ->
                let l = add_instr (SetLocal (k1, tmp1, next)) in
                let l = add_instr (Unop (tmp1, tr_unop op, tmp1, l)) in
                GetLocal (tmp1, k2, l)
          end
       end

    (* Cas d'un appel de fonction
       On règle à cet endroit les deux étapes du protocole d'appel incombant
       à l'appelant, à savoir :

       - Étape 1 : appelant, avant l'appel.
         Point principal : passage des arguments sur la pile, du dernier 
         au premier

            +------+
            |  aN  |
            +------+
            |      |
            |  ..  |
            |      |
            +------+
            |  a2  |
            +------+
            |  a1  |
            +------+
            |      |  <- nouvelle valeur de $sp (pointeur de sommet de pile)

         Cette première étape doit aussi se charger de sauvegarder les
         registres temporaires pour éviter qu'ils ne soient écrasés par la
         fonction appelée. Dans une version simple, on peut enregistrer tous
         les registres sur la pile avant de passer les arguments.

       - Étape 4 : appelant, après l'appel.
         Points principaux : récupérer la valeur renvoyée par l'appel, et
         retirer les arguments qui avaient été placés sur la pile avant 
         l'appel.
         Cette dernière étape doit aussi se charger de restaurer les
         registres temporaires qui avaient été sauvegardés avant l'appel.
     *)
    | Vips.Call(r, func, args, next) ->

        let call_label = 
          begin match func with
            | FName name -> (fun l -> add_instr (Call(FName name, l)))
            | FPointer r ->
              begin match Hashtbl.find alloc r with
                | Register r' -> (fun l -> add_instr (Call(FPointer r', l)))
                | Stacked k ->
                  (fun l ->
                    let j = add_instr (Call(FPointer tmp1, l)) in
                    add_instr (GetLocal(tmp1, k, j))
                  )
              end
          end in

       (* Protocole : étape 4 *)
       let retrieve_result = match Hashtbl.find alloc r with
         | Register r' -> Unop(r', Move, ret, next)
         | Stacked k   -> SetLocal(k, ret, next)
       in
       let l = add_instr retrieve_result in

       (* et restaurer les registres *)
       let retrieve_registers =
        List.fold_left (fun label r -> add_instr (Pop(r, label))) l (List.rev (Array.to_list registers)) in

        (* libérer l'espace de pile réservé pour les arguments *)
        let l = add_instr (Unop(sp, Addi (4*List.length args), sp, retrieve_registers)) in
       
       (* Appel *)
       let l = call_label l in
       (* à noter : l'appel va enregistrer l'adresse de retour dans le
          registre $ra *)
       
       (* Protocole : étape 1 *)
       let rec pass_args args l = match args with
         | [] -> l
         | a::args ->
          let l = begin match Hashtbl.find alloc a with
                    | Register r' -> add_instr (Push(r', l))
                    | Stacked k ->
                      let l = add_instr (Push(tmp1, l)) in
                      add_instr (GetLocal(tmp1, k, l))
                   end in
          pass_args args l
       in
       let l = pass_args args l in

       (* et sauvegarder les registres *)
       let save_registers =
        Array.fold_left (fun label r -> add_instr (Push(r, label))) l registers in

       Jump save_registers
       
    (* Cas de la fin d'exécution d'un appel de fonction
       On règle à cet endroit l'étape du protocole à la charge de l'appelé,
       à la fin de l'appel. Il faut
       - placer le résultat dans le registre dédié $v0 (désigné ici par [ret])
       - libérer l'espace de pile qui avait été réservé pour les variables
         locales de la fonction
       - restaurer les valeurs des registres $fp (adresse de référence du
         tableau d'activation de l'appelant) et $ra (adresse de retour qui
         avait été enregistrée au moment de l'appel).
     *)
    | Vips.Return r -> (* dans le protocole : 3 *)
       let l = add_instr Return in
       let l = add_instr (Pop(fp, l)) in
       let l = add_instr (Pop(ra, l)) in
       let l = add_instr (Unop(sp, Addi ~-8, fp, l)) in
       let i = match Hashtbl.find alloc r with
         | Register r' -> Unop(ret, Move, r', l)
         | Stacked k   -> GetLocal(ret, k, l)
       in
       i
    
    | GetGlobal (r, s, next) ->
      let f r s next = GetGlobal(r, s, next) in
      tr_virtual_register r f s next
    
    | Vips.SetGlobal (s, r, next) ->
      let f r s next = SetGlobal (s, r, next) in
      tr_virtual_register r f s next
    
    | Vips.GetParam (r, i, next) ->
      let f r i next = GetParam(r, i, next) in
      tr_virtual_register r f i next

    | Vips.Jump next -> Jump next
    
    | Vips.CJump (r, next1, next2) ->
      let f r next1 next2 = CJump(r, next1, next2) in
      tr_virtual_register r f next1 next2
    
    | Vips.Putchar (r, next) ->
      begin match Hashtbl.find alloc r with
       | Register r' ->
        let l = add_instr (Syscall next) in
        let l = add_instr (Cst(scod, 1, l)) in
        Unop (arg, Move, r', l)
       | Stacked k ->
        let l = add_instr (Syscall next) in
        let l = add_instr (Unop (scod, Addi 1, "$0", l)) in
        GetLocal (arg, k, l)
      end

    | Vips.Binop (r, b, r1, r2, next) ->
      let b = tr_binop b in
      begin match Hashtbl.find alloc r with
        | Register r' ->
          tr_virtual_binop r' r1 r2 b next
        | Stacked k ->
          let l = add_instr (SetLocal(k, tmp1, next)) in
          tr_virtual_binop tmp1 r1 r2 b l
      end
    
    | Vips.Write (r, r1, next) ->
      begin match Hashtbl.find alloc r with
        | Register r' ->
          begin match Hashtbl.find alloc r1 with
            | Register r1' -> Write (r', r1', next)
            | Stacked k1 ->
              let l = add_instr (Write (r', tmp1, next)) in
              GetLocal (tmp1, k1, l)
          end
        | Stacked k ->
          begin match Hashtbl.find alloc r1 with
              | Register r1' ->
                let l = add_instr (Write (tmp1, r1', next)) in
                GetLocal (tmp1, k, l)
              | Stacked k1 ->
                let l = add_instr (Write (tmp1, tmp2, next)) in
                let l = add_instr (GetLocal (tmp1, k, l)) in
                GetLocal (tmp2, k1, l)
            end
      end
    
    | Vips.Addr (r, id, next) ->
      begin match Hashtbl.find alloc r with
        | Register r' -> Addr (r', id, next)
        | Stacked k ->
          let l = add_instr (SetLocal (k, tmp1, next)) in
          Addr(tmp1, id, l)
      end
      
  in

  (* Boucle appliquant la traduction à chaque instruction du graphe de 
     la fonction VIPS prise en argument. Notez que c'est ici que 
     l'étiquette de chaque nœud est prélevée et réutilisée. *)
  Hashtbl.iter
    (fun l i -> Hashtbl.add code l (tr_instr i))
    Vips.(fdef.code);
  
  (* On ajoute en tête du code produit les instructions destinées à
     sauvegarder les registres $fp et $ra et allouer le tableau
     d'activation de la fonction. L'étiquette générée pour la première
     de ces instructions devient la nouvelle étiquette de départ. 

     C'est cette partie qui prend en charge l'étape 2 du protocole d'appel,
     à savoir la partie incombant à l'appelé, au début de l'appel.
   *)
  let l = add_instr (Unop(sp, Addi (-4 * nb_locals), sp,
                          Vips.(fdef.entry))) in
  let l = add_instr (Unop(fp, Addi 8, sp, l)) in
  let l = add_instr (Push(ra, l)) in
  let l = add_instr (Push(fp, l)) in

  (* J'ajoute à la main une instruction de saut ayant pour étiquette
     le nom d'origine de la fonction et qui saute vers l'étiquette
     renvoyée par la traduction.
     Cela permettra de faciliter la traduction des appels qui contiennent
     uniquement le nom de la fonction appelée
  *)
  Hashtbl.add code Vips.(fdef.name) (Jump l);

  {
    name = Vips.(fdef.name);
    code;
    entry = Vips.(fdef.name);
  }

(**
   Traduction d'un programme : simplement appliquer la traduction
   précédente à chaque fonction.
*)
let translate_prog prog = {
    globals = Vips.(prog.globals);
    functions = List.map translate_fdef Vips.(prog.functions)
}