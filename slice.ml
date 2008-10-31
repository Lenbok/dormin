let slice_oc = ref None

let add name data =
  match !slice_oc with
  | None -> ()
  | Some oc ->
      Format.printf "adding %s@." name;
      output_char oc '\000';
      output_string oc name;
      output_char oc '\000';
      output_string oc data;
;;

let openslice name =
  let oc = open_out_bin name in
  slice_oc := Some oc;
;;
