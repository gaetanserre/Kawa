(**
   Allocation de registres

   L'objectif est d'associer chaque registre virtuel à un emplacement
   mémoire réel, qui peut être :
   - dans l'idéal un registre physique
   - à défaut, un emplacement dans la pile

   Pour profiter au maximum des registres physiques et limiter 
   l'utilisation de mémoire, on essaie lorsque c'est possible d'associer
   plusieurs registres virtuels au même registre physique. C'est
   évidemment impossible pour deux registres virtuels dont les valeurs
   doivent être conservées simultanément en mémoire, mais c'est au
   contraire envisageable pour deux registres virtuels aux vivacités
   disjointes. L'allocation repose donc sur l'analyse de vivacité faite
   dans le fichier [liveness.ml]. 

   Le plan est le suivant :
   1. De l'analyse de vivacité, déduire un « graphe d'interférence »,
     c'est-à-dire un graphe dont les sommets sont les registres virtuels
     et dont les arêtes relient les registres virtuels dont les périodes 
     de vivacité s'intersectent (qui ne peuvent donc être réalisés par le
     même emplacement physique).
   2. Appliquer à ce graphe un algorithme de coloration : associer à 
     chaque sommet une « couleur » de sorte que deux sommets adjacents 
     aient des couleurs différentes. Les couleurs représentent ici les
     emplacement physiques utilisés pour réaliser les registres.
 *)
open Vips

(**
   On représentera les couleurs par des nombres entiers. Une coloration
   du graphe est alors une table associant chaque nœud à sa couleur, ou
   en l'occurrence chaque chaîne de caractères identifiant un nœud à
   l'entier représentant la couleur qui lui a été affectée.

   Pour la structure de table d'association, on utilise le foncteur
   [Map.Make]. Le principe est similaire à celui de [Set.Make]. Les
   fonctions de manipulation de ces tables seront de la forme 
   [C.<nom_de_fonction>].
   De même que pour les ensembles obtenus avec [Set.Make], les tables
   données par [Map.Make] sont immuables, chaque opération renverra donc
   une nouvelle table.
 *)
type color = int
module C = Map.Make(String)
type coloring = color C.t
module S = Set.Make(String)

(**
   Dans une étape finale, on traduira les numéros des couleurs en des
   emplacements réels (un registre physique, ou un numéro d'emplacement
   sur la pile). On définit à cette occasion la liste des registres
   MIPS que l'on va s'autoriser à utiliser pour l'allocation.
 *)           
type location = Register of string | Stacked of int
let registers =
  [| "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7";
     "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7" |]
let nb_registers = Array.length registers

(**
   Fonction de création du graphe d'interférence.

   Critère : si une instruction [i] écrit dans un registre [r], 
   alors tout registre autre que [r] vivant en sortie de [i] est
   en interférence avec [r].

   Exception : dans le cas d'une instruction [move] qui copie la valeur
   d'un registre [r] dans un registre [d], on n'ajoute pas de conflit
   entre [r] et [d] (au contraire, si ces deux registres virtuels étaient
   réalisés par le même emplacement, on économiserait cette instruction
   de copie !)
 *)
let make_interference_graph fdef =
  (* Calcul des vivacités (avec notre critère, on ne regarde que la
     vivacité en sortie des instructions) *)
  let _, live_out = Liveness.liveness fdef in

  (* On crée une référence sur un graphe initialement vide, qu'on mettra 
     à jour à mesure que la construction du graphe progresse. *)
  let graph = ref Graph.G.empty in
  
  (* Deuxième étape : ajout des sommets.
     Il faut parcourir les instructions et ajouter un sommet pour
     chaque registre virtuel rencontré. *)

  let add_nodes nl =
    List.iter (fun n -> graph := Graph.add_node n !graph) nl
  in

  let f _l i =
    match i with
      | Cst (r, _, _) | GetGlobal (r, _, _)
      | SetGlobal (_, r, _) | GetParam (r, _, _)
      | Putchar (r, _) | CJump (r, _, _)
      | Return r | Addr (r, _, _) ->
        add_nodes [r]
      | Unop (r1, _, r2, _) ->
        add_nodes [r1; r2]
      | Binop (r1, _, r2, r3, _) ->
        add_nodes [r1; r2; r3]
      | Call (r, FPointer rf, rs, _) ->
        add_nodes (r::rf::rs)
      | Call (r, FName _, rs, _) ->
        add_nodes (r::rs)
      | Jump _ -> ()
      | Write (r1, r2, _) ->
        add_nodes [r1; r2]
  in

  Hashtbl.iter f fdef.code;

  (* Troisième étape : ajout des arêtes.
     Il faut à nouveau parcourir les instructions et ajouter des arêtes
     en appliquant le critère d'interférence. *)
  
  let add_edge r l =
    let s = Hashtbl.find live_out l in
    Liveness.S.iter (fun r' -> graph := Graph.add_edge r r' !graph) s
  in

  let f l i =
    match i with
      | Cst (r, _, _) | GetGlobal (r, _, _)
      | GetParam (r, _, _) | Unop (r, _, _, _)
      | Binop (r, _, _, _, _) | Call (r, _, _, _) 
      | Addr (r, _, _) ->
        add_edge r l
      | _ -> ()
  in
  Hashtbl.iter f fdef.code;

  (* À la fin on renvoie le graphe qui a été construit. *)
  !graph

(* Fonction auxiliaire : choix du prochain nœud *)
exception Find_node of string
let choose_node g =
  let f node _ acc =
    let e_degree = Graph.degree node g in
    if e_degree < nb_registers then raise (Find_node node)
    else
      let degree, node = acc in
      if degree < nb_registers then acc
      else if e_degree > degree then (e_degree, node)
      else acc
  in
  try
    let _, node = Graph.G.fold f g (~-1, "") in
    node
  with Find_node node -> node

(* Fonction auxiliaire : choix d'une couleur pour un nœud *)
exception Color_already_picked
let pick_color x g coloring =
  let neighbors = Graph.find x g in
  let color = ref 0 in
  let flag = ref true in
  let f node =
    if not (C.mem node coloring) then ()
    else if C.find node coloring = !color then raise Color_already_picked;
  in
  while !flag do
    begin try
      Graph.S.iter f neighbors;
      flag := false;
    with Color_already_picked ->
      flag := true;
      incr color;
    end
  done;
  !color

(**
   Algorithme de coloration.

   Rappel : la coloration d'un graphe avec un nombre donné de couleurs
   est un problème NP-complet. On se repose donc sur des heuristiques
   plutôt que sur des algorithmes optimaux. On pourrait reprendre
   directement l'une des heuristiques classiques pour ce problème, mais
   on peut aussi obtenir de meilleurs résultats en en définissant une
   taillée sur mesure à notre instance du problème de coloration.
   
   On va tirer parti des remarques suivantes. Notons [g] le graphe que
   l'on cherche à colorer.

   1. Si un sommet [s] a un degré strictement inférieur à [nb_registers]
     (le nombre de registres physiques que l'on s'autorise à utiliser),
     alors on saura toujours affecter à [s] une couleur, quelle que soit
     la manière dont le reste du graphe aura été coloré. En effet, les 
     voisins de [s] ne bloqueront pas plus de [nb_registers-1] registres, 
     et il en restera forcément un disponible. On peut donc remettre le 
     choix d'une couleur pour [s] à plus tard.

     Pour réaliser cela, on commence par colorer le graphe [g'] obtenu 
     en retirant [s] de [g]. Pour étendre cette coloration de [g'] en
     une coloration de [g] il suffit de choisir une couleur pour [s] en
     fonction des couleurs choisies pour ses voisins dans [g'].

     Remarque : [g'] est obtenu en retirant un sommet [s] de [g], et les
     arêtes incidents à ce sommet [s]. Le degré des voisins de [s] est
     donc réduit dans [g'], ce qui ouvrira de nouvelles opportunités de
     trouver des sommets colorables à coup sûr.

   2. Si aucun sommet n'a un degré strictement inférieur à [nb_registers],
     alors on risque de ne pas trouver de registre physique disponible et
     de devoir utiliser à la place un emplacement de pile. On utilise la
     même technique récursive qu'au point précédent avec un sommet
     quelconque.

     Pour obtenir une coloration la meilleure possible on peut ajouter
     les heuristiques suivantes au choix du sommet à éliminer dans ce
     deuxième cas :
     - choisir un sommet de fort degré, pour éliminer un grand nombre
       d'interférences et faciliter le reste de la coloration,
     - choisir un sommet correspondant à un registre virtuel peu utilisé
       (le registre virtuel choisi dans ce deuxième cas est susceptible 
       d'être affecté sur la pile, et ses accès seront alors coûteux) 

   Pour limiter autant que possibles les copies, on peut ajouter les
   critères suivants : 
   - au moment de construire le graphe d'interférence, mémoriser les
     paires de registres virtuels pour lesquels le programme fait une
     copie (avec [move])
   - lorsqu'il y a une copie entre deux registres virtuels [r1] et [r2],
     que ces deux registres ne sont pas en interférence, et qu'ils ont à
     eux deux un degré strictement inférieur à [nb_registers], on peut
     fusionner leurs deux sommets dans [g] pour s'assurer que [r1] et 
     [r2] auront la même couleur (qui sera en l'occurrence celle d'un
     registre physique).
 *)
let rec color g =
  (** 
     Cas de base : si le graphe est vide on renvoie la coloration vide.
     Sinon on choisit un sommet [s] à éliminer :
     - de préférence un sommet de degré inférieur à [nb_registers],
     - sinon un sommet au hasard.
     On colorie le graphe obtenu en supprimant ce sommet [s].
     Enfin, on choisit une couleur pour [s].
   *)

   if Graph.G.is_empty g then C.empty
  else
    let node = choose_node g in
    let new_graph = Graph.remove_node node g in
    let coloring = color new_graph in
    let color = pick_color node g coloring in
    C.add node color coloring
  
  
(** 
   Fonction auxiliaire : conversion d'une coloration codée par des
   numéros en une table explicite
 *)
let explicit_allocation coloring =
  let a = Hashtbl.create 128 in
  C.iter
    (fun x i -> let r = if i < nb_registers then
                          Register(registers.(i))
                        else
                          Stacked(i-nb_registers)
                in Hashtbl.add a x r)
    coloring;
  a

(**
   Fonction auxiliaire : plus grand numéro utilisé dans une coloration.
 *)
let max x y =
  if x < y then y else x
let max_color coloring =
  C.fold (fun _ c maxc -> max c maxc) coloring 0


(**
   Fonction principale : création du graphe, coloration, puis conversion. 
   On renvoie également le nombre d'emplacements de pile utilisés, qui 
   servira à l'allocation du tableau d'activation de la fonction.
 *)
let allocate_function fdef =
  let g = make_interference_graph fdef in
  print_string "interference graphe ok\n";
  let coloring = color g in
  print_string "coloration ok\n";
  explicit_allocation coloring, max (max_color coloring - nb_registers) 0