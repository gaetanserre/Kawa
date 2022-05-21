(**
   Traduction de la représentation intermédiaire GIPS en assembleur MIPS.

   Le principal enjeu ici est de linéariser le code GIPS, c'est-à-dire 
   passer d'un graphe d'instructions élémentaires à une séquence
   d'instructions élémentaires (avec possiblement des étiquettes et des
   instructions de saut).

   Cet aspect mis à part, chaque instruction GIPS correspond assez
   directement à une ou deux instructions MIPS.
 *)
open Gips
open Mips

(** 
   Traduction du graphe d'une fonction GIPS en une séquence 
   d'instructions MIPS.
 *)   
let translate_fdef fdef =
  (**
     La traduction est essentiellement un parcours de graphe, commençant
     par le sommet d'entrée. Comme dans un parcours de graphe ordinaire,
     on évite les redondances et les boucles en marquant les sommets déjà 
     visités. Ici, on utilise une table de hachage des sommets déjà vus.
   *)
  let vus = Hashtbl.create 32 in

  let get_else_label =
    let count = ref 0 in
    fun () -> 
      let r = "else_" ^ string_of_int !count in
      incr count;
      r
  in

  let tr_binop binop r1 r2 r3 =
    let res = match binop with
      | Gips.Sub -> sub
      | Gips.Add -> add
      | Gips.Mul -> mul
      | Gips.Div -> div
      | Gips.Lt -> slt
      | Gips.Le -> sle
      | Gips.Gt -> sgt
      | Gips.Ge -> sge
      | Gips.Eq ->
        fun r1 r2 r3 ->
          subu r1 r2 r3
          @@ sltu r1 "$0" r1
          @@ xori r1 r1 1
    in
    res r1 r2 r3
  in

  let tr_unop unop r1 r2 =
    match unop with
    | Gips.Subi n -> subi r1 r2 n
    | Gips.Addi n -> addi r1 r2 n
    | Gips.ShiftL n -> sll r1 r2 n
    | Gips.ShiftR n -> srl r1 r2 n
    | Gips.Move -> move r1 r2
    | Gips.Read -> lw r1 0 r2
    in

  (**
     Fonction de traduction d'une expression
     - on commence par produire la ou les instructions MIPS correspondant
       à l'instruction GIPS traduite,
     - on passe ensuite à la génération des instructions du nœud suivant.
   *)
  let rec translate_instr = function
    | Gips.Cst(r, n, next) ->
       li r n @@ translate_label next
    
    | Gips.Unop(r1, u, r2, next) ->
      if r1 = r2 && u = Gips.Move then translate_label next
      else tr_unop u r1 r2 @@ translate_label next
    
    | Gips.Binop(r1, b, r2, r3, next) ->
      tr_binop b r1 r2 r3 @@ translate_label next
    
    | GetGlobal(r, s, next) ->
      la t1 s
      @@ lw r 0 t1
      @@ translate_label next
    
    | SetGlobal(s, r, next) ->
      la t1 s
      @@ sw r 0 t1
      @@ translate_label next
    
    | GetParam(r, i, next) ->
      lw r ((i+1)*4) fp @@ translate_label next

    | Gips.Jump next ->
       translate_label next
    
    | Gips.CJump(r, next1, next2) ->
      let else_label = get_else_label () in
      beqz r else_label 
        @@ translate_label next1
        @@ label else_label
        @@ translate_label next2
    
    | Gips.GetLocal(r, i, next) ->
      lw r ((i+1)*4) sp @@ translate_label next

    | Gips.SetLocal(i, r, next) ->
      sw r ((i+1)*4) sp @@ translate_label next
    
    | Call (FName f, next) ->
      jal f @@ translate_label next
    
    | Call (FPointer r, next) ->
      jalr r @@ translate_label next

    | Gips.Return ->
      jr ra
    
    | Gips.Push (r, next) ->
      sw r 0 sp @@ subi sp sp 4 @@ translate_label next
    
    | Gips.Pop (r, next) ->
      addi sp sp 4 @@ lw r 0 sp @@ translate_label next
          
    | Gips.Syscall next ->
      syscall @@ translate_label next
    
    | Gips.Write (r, r2, next) ->
      sw r2 0 r @@ translate_label next
    
    | Gips.Addr (r, id, next) ->
      la r id @@ translate_label next


  (**
     Boucle principale, travaillant sur une étiquette
   *)
  and translate_label l =
    (* Si l'étiquette a déjà été vue, cela signifie qu'on a déjà généré
       le code correspondant. Il suffit d'introduire un saut. *)
    if Hashtbl.mem vus l then
      b l
    else begin
      (* Sinon, on voit ce nœud pour la première fois : on le marque
         comme déjà vu et on génère le code correspondant. *)
      Hashtbl.add vus l ();
      label l @@ translate_instr (Hashtbl.find fdef.code l)
      end
  in

  (**
     On renvoie le code obtenu en commençant le parcours à l'étiquette
     d'entrée.
   *)
  translate_label fdef.entry
    
(**
   Traduction d'un programme complet. Vous pouvez y reconnaître ce qui 
   avait déjà été fait pour la traduction naïve [imp2mips] du premier 
   cours.
 *)
let translate_program prog =
    let init =
       beqz  a0 "init_end"
    @@ lw    a0 0 a1
    @@ jal   "atoi"
    @@ label "init_end"
    @@ sw    v0 0 sp
    @@ subi  sp sp 4
    @@ jal   "main"
    @@ li    v0 10
    @@ syscall
  and built_ins =
    comment "built-in atoi"
    @@ label "atoi"
    @@ li    v0 0
    @@ label "atoi_loop"
    @@ lbu   t0 0 a0
    @@ beqz  t0 "atoi_end"
    @@ addi  t0 t0 (-48)
    @@ bltz  t0 "atoi_error"
    @@ bgei  t0 10 "atoi_error"
    @@ muli  v0 v0 10
    @@ add   v0 v0 t0
    @@ addi  a0 a0 1
    @@ b     "atoi_loop"
    @@ label "atoi_error"
    @@ li    v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr    ra
  in
  
  let function_codes = List.fold_right
    (fun fdef code -> translate_fdef fdef @@ code)
    prog.functions nop
  in
  (* On commence par le code d'initialisation vu plus haut, puis on place 
     en vrac le code généré pour chaque fonction. *)
  let text = init @@ function_codes @@ built_ins
  (* Dans la partie "données", on introduit une étiquette pour chaque 
     variable globale. Chaque variable globale est initialisée à 0. *)
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  
  { text; data }
