open Format;;

type swztype = Plain8 | Plain4 | Swz8 | Swz4

external to_rgba
  : string -> (int * int) -> (int * int) -> swztype -> string
  = "ml_to_rgba"

let r xff sbufxff ?dim () =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let ntopos = xff.Xff.sections.(1).Xff.off in
  let ntobuf = Xff.sbufplus sbufxff ntopos in
  if not (Xff.cmp ntobuf (`chars "NTO2"))
  then
    Xff.sbuferr ntobuf 0 "invalid NTO signature"
  ;
  let pixpos = Xff.rint ntobuf 20 in
  let palpos = Xff.rint ntobuf 24 in
  let kind = Xff.r8 ntobuf 28 in
  let wh = Xff.r8 ntobuf 30 in
  let w, h =
    match dim with
    | None ->
        let h = 1 lsl (wh land 0xf)
        and w = 1 lsl (wh lsr 4) in
        (w, h)
    | Some (w, h) ->
        w, h
  in
  let mipmaps = Xff.r8 ntobuf 29 in
  let mipmaps = mipmaps lsr 4 in
  let swz = Xff.r8 ntobuf 31 in

  if false then
    printf "%dx%d kind=%d mipmaps=%d swz=%d@."
      w h kind mipmaps swz
  ;

  let to_rgba swz =
    let s, p = ntobuf in
    to_rgba s (p+pixpos, p+palpos) (w, h) swz
  in

  let rgba w h =
    match kind with
    | 0x00 ->                           (* 32 bit *)
        let len = w * h * 4 in
        let dst = String.create len in
        Xff.sbufblt ntobuf
          ~src_pos:pixpos
          ~dst
          ~dst_pos:0
          ~len
        ;
        dst

    | 0x14 when swz = 0            -> to_rgba Plain4
    | 0x14 when swz = 1 || swz = 3 -> to_rgba Swz4
    | 0x13 when swz = 0            -> to_rgba Plain8
    | 0x13 when swz = 1 || swz = 3 -> to_rgba Swz8

    | _ ->
        Xff.sbuferr ntobuf 28 "invalid kind"
  in
  Array.init mipmaps
    (fun i ->
      let w = w lsr i and h = h lsr i in
      w, h, rgba w h)
;;
