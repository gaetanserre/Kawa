(**
   Traduction de MIMP vers VIPS

   La différence entre les deux représentations est radicale, il y a donc 
   un peu plus de travail que dans la transformation précédente. 

   Il faut ici :

   - décomposer les expressions complexes en des séries d'opérations 
     élémentaires travaillant sur des registres virtuels (ces derniers
     étant introduits à la volée)

   - décomposer de même les instructions complexes en des opérations
     élémentaires, avec potentiellement des branchements ou des boucles

   - organiser le tout sous la forme d'un graphe

   La traduction se fait fonction par fonction, et on produit notamment
   un seul graphe par fonction.
 *)

open Vips

let mk_unop u =
  match u with
    | Mimp.Subi n -> Subi n
    | Mimp.Addi n -> Addi n
    | Mimp.ShiftL n -> ShiftL n
    | Mimp.ShiftR n -> ShiftR n
    | Mimp.Alloc -> Alloc
    | Mimp.Read -> Read

let mk_binop b =
  match b with
    | Mimp.Sub -> Sub
    | Mimp.Add -> Add
    | Mimp.Mul -> Mul
    | Mimp.Div -> Div
    | Mimp.Lt -> Lt
    | Mimp.Le -> Le
    | Mimp.Gt -> Gt
    | Mimp.Ge -> Ge
    | Mimp.Eq -> Eq

(** 
   Traduction d'une fonction
 *)
let translate_fdef f =

  (* Initialisation d'un graphe vide, qui sera rempli à mesure que l'on
     décidera des instructions à ajouter *)
  let code = Hashtbl.create 32 in
  (* Fonction auxiliaire de création d'une nouvelle étiquette *)
  let new_label =
    let f = Mimp.(f.name) in
    let count = ref 0 in
    fun () -> incr count; Printf.sprintf "%s_%i" f !count
  in
  (* Création d'un nouveau nœud du graphe.
       add_instr i
     ajoute au graphe un nœud contenant l'instruction [i] et renvoie
     l'étiquette de ce nœud (l'étiquette est créée à la volée et le
     graphe est modifié par effet de bord) *)
  let add_instr i =
    let l = new_label () in
    Hashtbl.add code l i;
    l
  in

  (* Une table qui associe chaque variable locale à un registre virtuel *)
  let locals = Hashtbl.create 8 in
  List.iter (fun x -> Hashtbl.add locals x (new_reg())) Mimp.(f.locals);

  (* Une table qui associe chaque paramètre de la fonction à son numéro *)
  let new_param =
    let count = ref 0 in
    fun () -> let r = !count in incr count; r
  in

  let params = Hashtbl.create 8 in
  List.iter (fun x -> Hashtbl.add params x (new_param ())) Mimp.(f.params);
  
  
  (**
     Traduction des expressions. 
     
     Paramètres :
       r     registre (virtuel) de destination de la valeur
       e     expression à traduire
       next  étiquette du nœud suivant

     Résultat : étiquette du premier nœud de l'expression
   *)
  let rec translate_expr r e next = match e with
    | Mimp.Cst n ->
       (* Cas d'une constante : une seule instruction suffit, on l'ajoute
          directement au graphe. *)
       add_instr (Cst(r, n, next))

    | Mimp.Var x ->
       (* Cas d'une variable : l'instruction produite varie en fonction 
          de la nature de cette variable (variable locale, paramètre de 
          fonction, variable globale. On consulte les tables pour avoir 
          cette information. *)
       if Hashtbl.mem locals x then
         (* Pour une variable locale, il suffit de la copier de son 
            registre [rx] vers le registre de destination souhaité [r] *)
         let rx = Hashtbl.find locals x in
         add_instr (Unop(r, Move, rx, next))
       else if Hashtbl.mem params x then
         (* Pour un paramètre ou une variable globale on utilise 
            l'instruction primitive adaptée *)
         let i = Hashtbl.find params x in
         add_instr (GetParam(r, i, next))
       else
         add_instr (GetGlobal(r, x, next))

    | Mimp.Unop(u, e') ->
       (* Cas d'une opération unaire. Deux points intéressants :
          - on crée un nouveau registre virtuel [tmp] pour la valeur de
            l'opérande [e'] (il s'agit d'un résultat intermédiaire du
            calcul)
          - comme la construction d'une instruction VIPS demande de déjà
            connaître l'étiquette de l'instruction suivante, il faut
            construire « à l'envers » *)
       let tmp = new_reg() in
       (* D'abord on construit l'opération elle-même, travaillant sur
          le registre [tmp], et on l'ajoute au graphe *)
       let i = Unop(r, mk_unop u, tmp, next) in
       let l = add_instr i in
       (* Puis on construit la partie du graphe correspondant au calcul
          de l'opérande [e'], en lui donnant comme suite l'instruction
          principale que l'on a déjà créée. *)
       translate_expr tmp e' l
       
    | Mimp.Binop(b, e1, e2) ->
       (* Cas d'une opération binaire. Similaire au précédent, mais 
          avec deux registres intermédiaires pour les valeurs des deux 
          opérandes. Il faut aussi créer une succession qui évalue
          bien [e1] et [e2] avant l'opération finale. L'ordre entre [e1]
          et [e2] peut être choisi. *)
       let tmp1 = new_reg () in 
       let tmp2 = new_reg () in
       let l = add_instr (Binop(r, mk_binop b, tmp1, tmp2, next)) in
       let l1 = translate_expr tmp1 e1 l in
       translate_expr tmp2 e2 l1
    | Mimp.Call (f, es) ->
      (* Cas d'un appel de fonction. Dans la lignée des deux précédents,
          on crée un registre intermédiaire pour la valeur de chaque
          argument de la fonction. Il faut produire le code évaluant et
          plaçant dans son registre virtuel chacun des arguments, puis
          conclure avec l'appel lui-même. L'évaluation des arguments est
          faite par la fonction auxiliaire [translate_args]. Ici, les
          arguments seront évalués de gauche à droite, mais la direction
          choisie importe peu. *)
      let rs = List.map (fun _ -> new_reg ()) es in
      let l = 
        begin match f with
          | Mimp.FName s ->
            add_instr (Call (r, FName s, rs, next))
          | Mimp.FPointer e ->
            let rf = new_reg () in
            let l = add_instr (Call (r, FPointer rf, rs, next)) in
            translate_expr rf e l
        end in
      if es = [] then l
          else
            let f acc r e = 
              let next = if acc = [] then l else List.hd acc in
              (translate_expr r e next) :: acc
            in
            let labels = List.fold_left2 f [] (List.rev rs) (List.rev es) in
            List.hd labels
    | Mimp.Seq (s, e) ->
      let l = translate_expr r e next in
      translate_seq s l
    | Mimp.Addr id -> add_instr (Addr (r, id, next))
       
  
  (**
     Traduction des instructions.
     
     Paramètres :
       i     instruction à traduire
       next  étiquette du nœud suivant

     Résultat : étiquette du premier nœud de l'instruction
   *)
  and translate_instr i next = match i with

    | Mimp.Putchar e -> 
      let r = new_reg () in
      let l = add_instr (Putchar (r, next)) in
      translate_expr r e l
    
    | Mimp.Set (s, e) ->
      let r = ref "" in
      let l = 
        if Hashtbl.mem locals s then
          (r := Hashtbl.find locals s; next)
        else if not (Hashtbl.mem params s) then
            (r := new_reg ();
            add_instr (SetGlobal (s, !r, next)))
        else
          failwith "Function's parameters are immutable"
        in
      translate_expr !r e l

    (* Branchement conditionnel *)
    | Mimp.If(e, s1, s2) ->
       (* On prévoit un registre virtuel pour le résultat de la
          condition [e] *)
       let r = new_reg () in
       (* Les séquences [s1] et [s2] prennent toutes deux [next]
          comme suite. C'est comme cela que l'on crée le point de
          jonction à la fin du branchement. *)
       let lthen = translate_seq s1 next in
       let lelse = translate_seq s2 next in
       (* Instruction de branchement, avec pour suite l'une des
          deux séquences produites au-dessus *)
       let ltest = add_instr (CJump(r, lthen, lelse)) in
       translate_expr r e ltest

    (* Boucle *)
    | Mimp.While(e, s) ->
       (* Registre virtuel pour le résultat de la condition [e] *)
       let r = new_reg () in
       (* Particularité ici : comme on crée un cycle dans le graphe
          on ne peut pas utiliser uniquement les étiquettes générées
          automatiquement par [add_instr]. On gère donc manuellement
          l'un des nœuds, en créant d'abord son étiquette, puis en
          définissant l'instruction associée plus tard. *)
       (* Étiquette pour l'instruction de branchement *)
       let ljump = new_label() in
       (* Traduction de la condition, donc la suite est l'instruction
          de branchement (pas encore créée mais pour laquelle on a
          déjà décidé l'étiquette) *)
       let le = translate_expr r e ljump in
       (* Traduction du corps de la boucle, avec pour suite
          l'évaluation de la condition *)
       let lloop = translate_seq s le in
       (* Création et ajout au graphe de l'instruction de 
          branchement, avec l'étiquette prévue.
          La suite « par défaut », en cas de test négatif, est
          l'étiquette [next]. *)
       Hashtbl.add code ljump (CJump(r, lloop, next));
       (* On renvoie l'étiquette par laquelle l'exécution doit
          commencer, à savoir l'évaluation de la condition *)
       le

    | Mimp.Return e ->
    (* Cas de la fin d'exécution d'une fonction. La valeur renvoyée
          passe comme les autres par un registre temporaire. *)
      let r = new_reg () in
      let l = add_instr (Return r) in
      translate_expr r e l

    | Expr e -> translate_expr (new_reg ()) e next

    | Write (e1, e2) ->
      let r1 = new_reg () in
      let r2 = new_reg () in
      let l = add_instr (Write (r1, r2, next)) in
      let l = translate_expr r1 e1 l in
      translate_expr r2 e2 l
    
  (* autres cas à traiter ! *)
       
  (**
     Traduction des séquences d'instructions.
     
     Paramètres :
       s     séquence à traduire
       next  étiquette du nœud suivant

     Résultat : étiquette du premier nœud de la séquence
   *)
  and translate_seq s next =
    let rec aux s =
      match s with
        | [] -> next
        | [i] -> translate_instr i next
        | i::ls -> 
          let l = aux ls in
          translate_instr i l
    in
    aux s
  in

  (**
     Code principal pour la traduction de la fonction.

     Il faut produire la structure
       { name; code; entry }

     Comme pour le reste de cette traduction, on démarre avec la fin du
     code de la fonction. En l'occurrence, on place une instruction 
     [return] vide, qui ne sera exécutée que si aucune instruction [return]
     n'est rencontrée dans le code. On prend ensuite l'étiquette de ce
     bloc final comme étiquette de "suite" de la séquence d'instructions
     définissant la fonction. L'étiquette [entry] est alors simplement
     l'étiquette de la première instruction produite par la traduction du
     corps de la fonction.
   *)
  let tmp = new_reg() in
  let l = add_instr (Return tmp) in
  let l = add_instr (Cst(tmp, 0, l)) in
  let entry = translate_seq Mimp.(f.code) l in

  {
    name = Mimp.(f.name);
    code;
    entry
  }
  

(**
   Traduction d'un programme complet.
   Il suffit d'appliquer la traduction à chaque fonction.
 *)
let translate_prog p = {
    globals = Mimp.(p.globals);
    functions = List.map translate_fdef Mimp.(p.functions)
  }
