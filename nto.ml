open Format;;

type swztype = Plain8 | Plain4 | Swz8 | Swz4

external to_rgba
  : string -> (int * int) -> (int * int) -> swztype -> string
  = "ml_to_rgba"

let r xff sbufxff =
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
  let w = 1 lsl (wh land 0xf)
  and h = 1 lsl (wh lsr 4) in
  let mipmaps = Xff.r8 ntobuf 29 in
  let mipmaps = mipmaps lsr 4 in
  let swz = Xff.r8 ntobuf 31 in

  if false then
    printf "%dx%d kind=%d mipmaps=%d swz=%d@."
      w h kind mipmaps swz
  ;

  let to_rgba pixpos w h swz =
    let s, p = ntobuf in
    to_rgba s (p+pixpos, p+palpos) (w, h) swz
  in

  let rgba level pixpos w h =
    let swizzled = (1 lsl level) land swz != 0 in
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
        dst, len

    | 0x14 -> to_rgba pixpos w h (if swizzled then Swz4 else Plain4), w*h/2
    | 0x13 -> to_rgba pixpos w h (if swizzled then Swz8 else Plain8), w*h

    | _ ->
        Xff.sbuferr ntobuf 28 "invalid kind"
  in
  let pixpos = ref pixpos in
  Array.init mipmaps
    (fun i ->
      let w = w lsr i and h = h lsr i in
      let data, insize = rgba i !pixpos w h in
      let v = w, h, data in
      pixpos := !pixpos + insize;
      v
    )
;;
