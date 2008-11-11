let slice = ref `none

let add name data =
  match !slice with
  | `none -> ()
  | `oc oc ->
      Format.printf "adding %s@." name;
      output_char oc '\000';
      output_string oc name;
      output_char oc '\000';
      output_string oc data;
  | `dir dir ->
      let oc = open_out_bin (Filename.concat dir name) in
      output_string oc data;
      close_out oc
;;

let openslice name =
  if Sys.is_directory name
  then
    slice := `dir name
  else
    let oc = open_out_bin name in
    slice := `oc oc;
;;
