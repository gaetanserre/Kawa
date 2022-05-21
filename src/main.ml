open Format

let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Kawaparser.program Kawalexer.token lb in
  close_in c;
  print_string "parsing ok\n";
  let pimp = Kawa2pimp.tr_prog prog in
  print_string "traduction to pimp ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".pimp" in
  let out = open_out output_file in
  Pimppp.pp_program pimp out;
  close_out out;
  let mimp = Pimp2mimp.isel_prog pimp in
  print_string "instruction selection ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".mimp" in
  let out = open_out output_file in
  Mimppp.pp_program mimp out;
  close_out out;
  let vips = Mimp2vips.translate_prog mimp in
  print_string "decomposition ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".vips" in
  let out = open_out output_file in
  Vipspp.pp_program vips out;
  close_out out;
  Deadwriteselimination.dwe_prog vips;
  print_string "dead writes elimination ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".vipsopt" in
  let out = open_out output_file in
  Vipspp.pp_program vips out;
  close_out out;
  let gips = Vips2gips.translate_prog vips in
  print_string "register allocation ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".gips" in
  let out = open_out output_file in
  Gipspp.pp_program gips out;
  close_out out;
  let asm = Gips2mips.translate_program gips in
  print_string "assembly generation ok\n";
  let output_file = (Filename.chop_suffix file ".pimp") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
