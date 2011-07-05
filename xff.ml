type offsets =
    { off1 : int
    ; symstrpos : int
    ; symstr : int
    ; sec : int
    ; sym : int
    ; off2 : int
    ; secstrpos : int
    ; secstr : int
    }

type section =
    { name : string
    ; len : int
    ; off : int
    }

type xff =
    { size : int
    ; entry : int
    ; offsets : offsets
    ; sections : section array
    }

let sbufplus sbuf pos =
  let s, pos' = sbuf in
  s, pos' + pos
;;

let sbufpos (_, pos) = pos;;

let sbufblt sbuf ~src_pos ~dst ~dst_pos ~len =
  let (src, pos) = sbuf in
  let src_pos = src_pos + pos in
  StringLabels.blit
    ~src
    ~dst
    ~src_pos
    ~dst_pos
    ~len
;;

let sbuferr sbuf pos msg =
  let p = sbufpos sbuf in
  let s = Printf.sprintf "%08x(%08x): %s" p (p+pos) msg in
  failwith s
;;

let r32 sbuf pos =
  let s, pos1 = sbuf in
  let rb pos2 = Char.code (String.get s (pos + pos1 + pos2)) in
  let w0 = (rb 0) lor ((rb 1) lsl 8)
  and w1 = (rb 2) lor ((rb 3) lsl 8) in
  let u = Int32.shift_left (Int32.of_int w1) 16
  and l = Int32.of_int w0 in
  Int32.logor u l
;;

let r16 sbuf pos =
  let s, pos1 = sbuf in
  let rb pos2 = Char.code (String.get s (pos + pos1 + pos2)) in
  (rb 0) lor ((rb 1) lsl 8)
;;

let r16s sbuf pos =
  let v = r16 sbuf pos in
  v - ((v land 0x8000) lsl 1)
;;

let rchar sbuf pos =
  let s, pos1 = sbuf in
  String.get s (pos + pos1)
;;

let r8 sbuf pos =
  let s, pos1 = sbuf in
  Char.code (String.get s (pos + pos1))
;;

let rint sbuf pos =
  let i32 = r32 sbuf pos in
  Int32.to_int i32
;;

let rfloat sbuf pos =
  let i32 = r32 sbuf pos in
  Int32.float_of_bits i32
;;

let checkc sbuf n c =
  let s, pos = sbuf in
  let c' = String.get s (pos+n) in
  c' = c
;;

let check32 sbuf n i32 =
  let i32' = r32 sbuf (n*4) in
  i32' = i32
;;

let checklist cf sbuf l =
  let rec f n = function
    | [] -> true
    | e :: rest when cf sbuf n e -> f (n+1) rest
    | _ -> false
  in
  f 0 l
;;

let checkstr cf sbuf s =
  let l = String.length s in
  let rec f = function
    | 0 -> true
    | n when cf sbuf (l-n) s.[l-n] -> f (n-1)
    | _ -> false
  in
  f l
;;

let cmp sbuf = function
  | `chars s -> checkstr checkc sbuf s
  | `dwords l -> checklist check32 sbuf l
;;

let rcstrtabent sbuf pos at =
  let (src, pos1) = sbuf in
  let begpos = pos1 + pos + at in
  let endpos = String.index_from src begpos '\000' in
  let len = endpos - begpos in
  let dst = String.create len in
  StringLabels.blit
    ~src
    ~dst
    ~src_pos:begpos
    ~dst_pos:0
    ~len
  ;
  dst
;;

let roffsets sbuf pos =
  let ri n = rint sbuf (pos + n*4) in
  { off1 = ri 0
  ; symstrpos = ri 1
  ; symstr = ri 2
  ; sec = ri 3
  ; sym = ri 4
  ; off2 = ri 5
  ; secstrpos = ri 6
  ; secstr = ri 7
  }
;;

let rstrpos sbuf pos count =
  let r n = rint sbuf (pos + n*4) in
  Array.init count r
;;

let rsection sbuf offs secstrpos index =
  let secpos = offs.sec + index*8*4 in
  let len = rint sbuf (secpos + 2*4)
  and off = rint sbuf (secpos + 7*4)
  and name = rcstrtabent sbuf offs.secstr (Array.get secstrpos index) in
  { name = name
  ; len = len
  ; off = off
  }
;;

let rxff ic =
  let s = String.create 0x50 in
  let sbuf = (s, 0) in
  let () = really_input ic s 0 0x50 in
  let rc n = String.get s n in

  if rc 0 <> 'x' || rc 1 <> 'f' || rc 2 <> 'f' ||
    (let c = rc 3 in not (c == '2' || c == '\000'))
  then
    failwith "Not an xff"
  ;

  let size = rint sbuf (5*4)
  and entry = rint sbuf (19*4)
  and seccount = rint sbuf (16*4) in

  let sbuf =
    let s' = String.create size in
    let () = really_input ic s' 0x50 (size-0x50) in
    StringLabels.blit
      ~src:s
      ~src_pos:0
      ~dst:s'
      ~dst_pos:0
      ~len:0x50
    ;
    (s', 0)
  in

  let offsets = roffsets sbuf 0x50 in

  let secstrpos = rstrpos sbuf offsets.secstrpos seccount in
  let sections = Array.init seccount (fun n -> rsection sbuf offsets secstrpos n) in
  { size = size
  ; entry = entry
  ; offsets = offsets
  ; sections = sections
  }, sbuf
;;

let test path =
  let ic = open_in_bin path in
  let r = rxff ic in
  close_in ic;
  r
;;

let index_path = ref "index/index";;
let base_path = ref None;;

let index =
  lazy
    (
      let ic = open_in_bin !index_path in
      let l = input_value ic in
      close_in ic;
      l
    )
;;

let test2 name =
  let (path, off, len) =
    let rec find = function
      | [] -> failwith ("can't find " ^ name)
      | (path, h) :: rest ->
          try
            let off, len = Hashtbl.find h name in
            path, off, len
          with Not_found ->
            find rest
    in
    find (Lazy.force index)
  in
  let path =
    match !base_path with
    | Some base -> Filename.concat base (Filename.basename path)
    | None -> path
  in
  if false then Format.eprintf "%s %s %08x %d@." name path off len;
  let off =
    if off lsr 30 = 1
    then Int64.logor (Int64.of_int (off land 0x3fff_ffff)) 0x4000_0000L
    else Int64.of_int off
  in
  let ic = open_in_bin path in
  LargeFile.seek_in ic off;
  let (_, (s, _)) as r = rxff ic in
  close_in ic;
  Slice.add name s;
  r
;;
