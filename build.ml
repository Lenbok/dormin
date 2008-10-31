let start = Unix.gettimeofday ();;
open List;;
open Typs;;
open Utils;;
open State;;
open Helpers;;

let jobs, targets, dodeplist, dotarlist = getopt ();;

let srcdir =
  match getval "src" with
  | None -> failwith "no source dir"
  | Some s -> s
;;

let cc =
  match getval "cc" with
  | None -> "cc"
  | Some s -> s
;;

let boc flags src =
  let o = src ^ ".o" in
  let c = src ^ ".c" in
  ocaml
    "ocamlc.opt"
    ("-cc " ^ cc ^ " -ccopt '-Wall -Wno-unused " ^ flags ^ " -o " ^ o ^ "'")
    o
    (StrSet.singleton o)
    [Filename.concat srcdir c]
    (
      if src = "skin"
      then StrSet.singleton (Filename.concat srcdir "vec.c")
      else StrSet.empty
    )
  ;
;;

let bso src =
  let so = src ^ ".so" in
  let so = Filename.concat (Sys.getcwd ()) so in
  let o = src ^ ".o" in
  ocaml
    cc
    ("-shared -o " ^ so)
    so
    (StrSet.singleton so)
    [o]
    StrSet.empty
  ;
  so
;;

let _ =
  List.iter (fun src ->
    cmopp ~flags:"-g -I +lablGL -thread" ~dirname:srcdir src)
    ["xff"; "nto"; "nmo"; "slice"; "rend"; "vec"; "skb"; "qtr"; "anb"; "skin"]
  ;
  boc "-g -O" "swizzle";
  boc "-g -O" "skin";
  let so = bso "swizzle" in
  let so1 = bso "skin" in
  let prog name cmos =
    ocaml
      "ocamlc.opt"
      ("-g -I +lablGL lablgl.cma lablglut.cma unix.cma")
      name
      (StrSet.singleton name)
      (State.dep_sort cmos)
      StrSet.empty
  in
  prog "dormin" ["slice.cmo"; "xff.cmo"; "nto.cmo"; "rend.cmo";
                 "vec.cmo"; "anb.cmo"; "skb.cmo"; "skin.cmo";
                 "nmo.cmo"; "qtr.cmo";
                 so; so1];
  ()
;;

let () =
  Helpers.run start jobs targets dodeplist dotarlist
;;
