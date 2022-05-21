open Pimp

exception Found of Kawa.class_def
exception Override

(** Tables qui, pour chaque classe, associe une table qui, pour chaque attribut/méthode, associe son offset*)
type class_offset = (string, int) Hashtbl.t
let (class_attribute_offset: (string, class_offset) Hashtbl.t) = Hashtbl.create 10
let (class_method_offset: (string, class_offset) Hashtbl.t) = Hashtbl.create 10

(** Table qui, pour chaque variable associe son type *)
let (var_type: (string, Kawa.typ) Hashtbl.t) = Hashtbl.create 10

(** Tables qui, pour chaque classe, associe une table qui, pour chaque attribut/méthode, associe son type *)
type table_type = (string, Kawa.typ) Hashtbl.t
let (class_attr_type: (string, table_type) Hashtbl.t) = Hashtbl.create 10
let (class_meth_type: (string, table_type) Hashtbl.t) = Hashtbl.create 10

(** Table qui, pour chaque classe, contient le nom de son 'super' constructeur *)
let (class_super_name: (string, string) Hashtbl.t) = Hashtbl.create 10

(** 
 * Table qui, pour chaque classe, associe la liste des 
 * noms des méthodes de son descripteur (en tenant compte des héritages).
 * La liste est sous la forme:
 * (nom de la méthode, nom de la classe qui l'implémente) list.
*)
let (class_desc_meth_name: (string, (string * string) list) Hashtbl.t) = Hashtbl.create 10

let size_int = 4

let current_class = ref ""

let add_type table vl = 
  List.iter (fun var -> Hashtbl.add table (fst var) (snd var)) vl

let remove_type table vl =
  List.iter (fun var -> Hashtbl.remove table (fst var)) vl

let add_var vl =
  add_type var_type vl

let remove_var vl =
  remove_type var_type vl

let add_type table c_name al =
  if not (Hashtbl.mem table c_name) then
    Hashtbl.add table c_name (Hashtbl.create 5);

  let t = Hashtbl.find table c_name in
  List.iter (fun a -> Hashtbl.add t (fst a) (snd a)) al

let add_attribute_type c_name al =
  add_type class_attr_type c_name al

let add_method_type c_name ml =
  let ml = List.fold_left (fun acc (m_def: Kawa.method_def) ->
                            (m_def.method_name, m_def.return) :: acc)
                          [] ml in
  add_type class_meth_type c_name ml


let get_descriptor c_name =
  "descriptor_" ^ c_name

let get_tmp_var =
  let count = ref 0 in
  fun () -> incr count; "tmp_" ^ string_of_int !count

(** Fonction qui renvoie la classe de l'expression expr *)
let rec get_class (expr: Kawa.expr) =
  let find_class table p =
    match Hashtbl.find table p with
      | Kawa.Typ_Class s -> s
      | _ -> failwith "Not a class"
  in

  match expr with
    | Kawa.Get mem ->
      begin match mem with
        | Kawa.Var p ->
          if Hashtbl.mem var_type p then find_class var_type p
          else
            (
            let attr_tbl = Hashtbl.find class_attr_type !current_class in
            if Hashtbl.mem attr_tbl p then find_class attr_tbl p
            else failwith "Variable not found")
        | Kawa.Field(e, a) ->
          let c = get_class e in
          let attr_tbl = Hashtbl.find class_attr_type c in
          if Hashtbl.mem attr_tbl a then find_class attr_tbl a
          else failwith "Attribute not found"
      end
    | Kawa.This -> !current_class
    | Kawa.MethCall (e, m, el) ->
      let c = get_class e in
      let meth_tbl = Hashtbl.find class_meth_type c in
      if Hashtbl.mem meth_tbl m then find_class meth_tbl m
      else failwith "Method not found"
    | _ -> failwith "Cannot get attribute from operations"

let get_attribute_address (expr: Kawa.expr) (expr_pimp: Pimp.expression) (attribute: string) =
  let c_name = get_class expr in
  let offset = Hashtbl.find class_attribute_offset c_name in
  let offset = Hashtbl.find offset attribute in
  Binop (Add, expr_pimp, Cst ((offset + 1) * size_int))
  
let get_method_address (expr: Kawa.expr) (expr_pimp: Pimp.expression) (meth: string) =
  let c_name = get_class expr in
  let offset = Hashtbl.find class_method_offset c_name in
  let offset = Hashtbl.find offset meth in
  Unop (Read, Binop (Add, Unop (Read, expr_pimp), Cst (offset * size_int)))

let add_locals = ref (fun (s: string) (t: Kawa.typ): unit -> ())

let tr_binop (b: Kawa.binop): Pimp.binop =
  match b with
    | Kawa.Sub -> Sub
    | Kawa.Add -> Add
    | Kawa.Mul -> Mul
    | Kawa.Div -> Div
    | Kawa.Lt  -> Lt
    | Kawa.Le  -> Le
    | Kawa.Gt  -> Gt
    | Kawa.Ge  -> Ge
    | Kawa.Eq  -> Eq

let rec tr_expr (expr: Kawa.expr): Pimp.expression =
  match expr with
    | Kawa.Cst n -> Cst n
    | Kawa.Bool b -> Bool b
    | Kawa.Binop (b, e1, e2) ->
      Binop (tr_binop b, tr_expr e1, tr_expr e2)
    | Kawa.Get mem ->
      begin match mem with
        | Var s -> Var s
        | Field (e, attr) ->
          Unop (Read, get_attribute_address e (tr_expr e) attr)
      end
    | Kawa.This -> Var "this"
    | Kawa.New (obj, el) ->
      (**
       * On créé une nouvelle variable locale qui va recevoir
       * le pointeur du bloc mémoire décrivant le nouvel objet 
      *)
      let tmp_var = get_tmp_var () in
      !add_locals tmp_var (Typ_Class obj);

      (** On alloue le bloc mémoire  *)
      let nb_attribute = Hashtbl.length (Hashtbl.find class_attribute_offset obj) in
      let alloc_bloc = Set (tmp_var, Unop(Alloc, (Cst ((nb_attribute + 1) * size_int)))) in
      let write_descriptor = Write (Var tmp_var, Var (get_descriptor obj)) in
      Seq (
            [alloc_bloc;
             write_descriptor;
             Expr(Call (FName (obj^"constructor"), Var tmp_var :: (List.map (fun e -> tr_expr e) el)))],
            Var tmp_var
          )

    | Kawa.MethCall (e, m, el) ->
      let expr_pimp = tr_expr e in
      let args = expr_pimp :: List.map (fun e -> tr_expr e) el in
      if m = "super" then
        let class_name = get_class e in
        let m = 
          match Hashtbl.find_opt class_super_name class_name with
            | Some m -> m
            | None -> failwith "This class has no super constructor"
        in
        Printf.printf "super: %s\n" m;
        Call (FName m, args)
      else
        let address = get_method_address e expr_pimp m in
        Call (FPointer address, args)

let rec tr_instr (instr: Kawa.instr): Pimp.instruction =
  match instr with
    | Kawa.Putchar e             -> Putchar (tr_expr e)
    | Kawa.If (cond, seq1, seq2) -> If (tr_expr cond, tr_seq seq1, tr_seq seq2)
    | Kawa.While (cond, seq)     -> While (tr_expr cond, tr_seq seq)
    | Kawa.Return e              -> Return (tr_expr e)
    | Kawa.Expr e                -> Expr (tr_expr e)
    | Kawa.Set (mem, e)          ->
      begin match mem with
        | Var s -> Set(s, tr_expr e)
        | Field (obj, attr) ->
          let address = get_attribute_address obj (tr_expr obj) attr in
          Write (address, tr_expr e)
      end

and tr_seq (seq: Kawa.seq): Pimp.sequence =
  let rec aux seq acc =
    match seq with
      | [] -> acc
      | i :: il -> aux il (tr_instr i :: acc)
  in
  List.rev (aux seq [])

let tr_method (c_def: Kawa.class_def) (m_def: Kawa.method_def): Pimp.function_def =

  let locals = ref m_def.locals in
  add_locals := (fun s t -> locals := (s, t) :: !locals);

  let params = (("this", Kawa.Typ_Class c_def.class_name) :: m_def.params) in
  add_var params;
  add_var m_def.locals;

  let code_pimp = tr_seq m_def.code in
  let params_pimp = List.map (fun p -> fst p) params in
  let f_def = {
    name = c_def.class_name ^ m_def.method_name;
    code = code_pimp;
    params = params_pimp;
    locals = List.map (fun p -> fst p) !locals;
  } in
  remove_var params;
  remove_var m_def.locals;
  f_def

let tr_class (prog: Kawa.program) (c_def: Kawa.class_def): (Pimp.function_def list * Pimp.instruction list) =

  let define_class_offset (c_def: Kawa.class_def): unit =
    (**
    * Fonction qui renvoie la valeur de dernier offset utilisé
    * et ajoute dans la table les attributs/méthodes hérité(e)s 
    *)
    let handle_heritage_offset cname table table_child =
      let c_def = List.find (fun (c_def: Kawa.class_def) -> c_def.class_name = cname) prog.classes in
      match c_def.parent with
      | None -> ~-1
      | Some parent ->
        let count = ref ~-1 in
        let table_parent = Hashtbl.find table parent in
        Hashtbl.iter (fun s off -> Hashtbl.add table_child s off; count := max off !count) table_parent;
        !count
    in

    (** 
    * Si un offset a déjà été attribué à la méthode ou à l'attribut k (dans le cas d'une surcharge)
    * alors on le passe 
    *)
    let set_offset table k offset =
      if Hashtbl.mem table k then
        (if k = "constructor" then incr offset
        else ())
      else (incr offset; Hashtbl.add table k !offset)
    in

    add_attribute_type c_def.class_name c_def.attributes;
    add_method_type c_def.class_name c_def.methods;

    (** Initialise les offsets des attributs *)
    let (attributes: class_offset) = Hashtbl.create 5 in
    let count = ref (handle_heritage_offset c_def.class_name class_attribute_offset attributes) in
    List.iter (fun attr -> set_offset attributes (fst attr) count)
              c_def.attributes;
    Hashtbl.add class_attribute_offset c_def.class_name attributes;

    (** Initialise les offsets des méthodes *)
    let (methods: class_offset) = Hashtbl.create 5 in
    count := handle_heritage_offset c_def.class_name class_method_offset methods;
    List.iter
      (fun (meth: Kawa.method_def) -> set_offset methods meth.method_name count)
      c_def.methods;
    Hashtbl.add class_method_offset c_def.class_name methods;
  in

  let get_method_class_assoc c_name ml =
    let r = List.fold_left (fun acc (m: Kawa.method_def) -> (m.method_name, c_name) :: acc) [] ml in
    List.rev r
  in

  let check_override ml_iter ml_check callback1 callback2 =
    let f acc (m, c) =
      let f' (m', c') =
        if m = m' then raise Override
      in
      try
        if m <> "constructor" then List.iter f' ml_check;
        callback1 m c acc
      with Override ->
        callback2 m c acc
    in
    let r = List.fold_left f [] ml_iter in
    List.rev r
  in

  let get_parent_method_names c_name mlp mlc =
    let callback1 m c acc =
      (m, c) :: acc
    in
    let callback2 m _c acc =
      (m, c_name) :: acc
    in
    check_override mlp mlc callback1 callback2
  in

  let get_child_method_names mlp mlc =
    let callback1 m c acc =
      (m, c) :: acc
    in
    let callback2 _m _c acc =
      acc
    in
    check_override mlc mlp callback1 callback2
  in

  (** 
   * Créé la liste des noms de toutes les méthodes de cette classe (héritées ou non)
   * dans le bon ordre (méthodes des classes mères suivies de celles de la classe fille).
   * Les fonction get_parent_method_names et get_child_method_names calculent
   * quelles sont les méthodes surchargées et en tiennent compte pour créer la liste.
  *)
  let method_name_list =
    let l = match c_def.parent with
      | None -> get_method_class_assoc c_def.class_name c_def.methods
      | Some parent ->
        (** On en profite pour remplir la table des constructeurs 'super' *)
        Hashtbl.add class_super_name c_def.class_name (parent ^ "constructor");
        let methods_parent = Hashtbl.find class_desc_meth_name parent in
        let methods_child = get_method_class_assoc c_def.class_name c_def.methods in
        let methods_parent_final = get_parent_method_names c_def.class_name methods_parent methods_child in
        let methods_child = get_child_method_names methods_parent methods_child in
        methods_parent_final @ methods_child
    in
    Hashtbl.add class_desc_meth_name c_def.class_name l;
    List.map (fun (m, c) -> c ^ m) l
  in

  Printf.printf "%s\n" c_def.class_name;
  List.iter(fun m -> Printf.printf "%s " m) method_name_list;
  Printf.printf "\n";

  (** Créé le descripteur de la classe *)
  let descriptor_name = get_descriptor c_def.class_name in
  let descriptor = ref [Set(descriptor_name, Unop (Alloc, Cst (size_int * List.length method_name_list)))] in
  (** Fonction qui va écrire l'adresse de chaque méthode au bon endroit dans le descripteur *)
  let write_method_address =
    let count = ref ~-1 in
    fun m_name ->
      incr count;
      let instr = Write (Binop(Add, Var descriptor_name, Cst (!count * size_int)), Addr m_name) in
      descriptor := instr :: !descriptor;
  in

  List.iter write_method_address method_name_list;

  current_class := c_def.class_name;
  define_class_offset c_def;
  let funcs = List.map (fun (m: Kawa.method_def) -> tr_method c_def m) c_def.methods in
  funcs, List.rev !descriptor

let tr_prog (prog: Kawa.program): Pimp.program =
  add_var prog.globals;
  let init_descriptors = ref [] in
  let descriptor_names = ref [] in
  (** 
   * Fonction qui va créer la liste des noms des descripteurs de classe
   * ainsi que la liste des instructions nécessaire pour les initialiser.
   * Elle va aussi traduire les classes en fonctions Pimp.
  *)
  let f acc c_def =
    let funcs, descriptor = tr_class prog c_def in
    descriptor_names := !descriptor_names@[get_descriptor c_def.class_name];
    init_descriptors := !init_descriptors @ descriptor;
    acc @ funcs
  in
  let functions = List.fold_left f [] prog.classes in

  let locals = ref [] in
  add_locals := (fun s t -> locals := (s, t) :: !locals);
  let code_pimp = tr_seq prog.main in

  add_var [("a", Kawa.Typ_Int)];

  let main = {
    name = "main";
    code = !init_descriptors @ code_pimp;
    params = ["a"];
    locals = List.map (fun p -> fst p) !locals;
  } in
  let functions = main :: functions in
  let globals = List.map (fun p -> fst p) prog.globals in

  {
    functions = functions;
    globals = !descriptor_names @ globals;
  }