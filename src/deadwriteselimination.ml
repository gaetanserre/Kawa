open Vips
open Liveness

let dwe_step fdef =
  let flag = ref false in
  let _, live_out = liveness fdef in
  let f label instr =
    match instr with
      | Cst(r, _, next) | Unop(r, _, _, next)
      | Binop(r, _, _, _, next) | GetGlobal(r, _, next)
      | GetParam(r, _, next) | Addr(r, _, next) ->
        let s = Hashtbl.find live_out label in
        if S.mem r s then ()
        else
          (flag := true;
          Hashtbl.replace fdef.code label (Jump next))
      | _ -> ()
  in
  Hashtbl.iter f fdef.code;
  !flag

let dwe fdef =
 while dwe_step fdef do () done

let dwe_prog prog =
  List.iter (fun f_def -> dwe f_def) prog.functions