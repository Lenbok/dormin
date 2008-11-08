open Format

let quat0 posecount sbuf =
  let i = Xff.rfloat sbuf 0
  and j = Xff.rfloat sbuf 4
  and k = Xff.rfloat sbuf 8
  and s = Xff.rfloat sbuf 12 in
  let q = { Qtr.i = i; j = j; k = k; s = s } in
  Array.create posecount q
;;

let expand =
  let bits =
    [|[|5; 9; 9; 9|]; [|9; 5; 9; 9|]; [|9; 9; 5; 9|]; [|9; 9; 9; 5|]|]
  in
  fun int32 typ ->
    let toint shift mask =
      let i = (Int32.to_int (Int32.shift_right_logical int32 shift)) land mask in
      let i = i - ((i land ((mask + 1) lsr 1)) lsl 1) in
      i
    in
    let bits = bits.(typ - 3) in
    let shifts =
      [| 32 - bits.(0)
      ;  32 - bits.(0) - bits.(1)
      ;  32 - bits.(0) - bits.(1) - bits.(2)
      ;  0
      |]
    in
    let toint i = (toint shifts.(i) ((1 lsl bits.(i)) - 1)) in
    let tofloat i = float (toint i) /. float (1 lsl (bits.(i) - 1)) in
    let a = tofloat 0
    and b = tofloat 1
    and c = tofloat 2
    and d = tofloat 3 in
    (a, b, c, d, toint (typ - 3))
;;

let quat3456 typ posecount sectbuf sbuf =
  let floats = Array.init 8 (fun i -> Xff.rfloat sbuf (i*4)) in
  let offset = Xff.rint sbuf 32 in
  let sbuf32 = Xff.sbufplus sectbuf offset in
  let int32s = Array.init posecount (fun i -> Xff.r32 sbuf32 (i*4)) in
  let madd v n = v*.floats.(n*2 + 1) +. floats.(n*2) in
  let omsqrt a b c mask =
    let m = a*.a +. b*.b +. c*.c in
    let v = if m >= 1.0 then 0.0 else sqrt (1. -. m) in
    if mask land 0b10000 != 0 then -.v else v
  in
  let toquat poseno =
    let i, j, k, s, mask = expand int32s.(poseno) typ in
    let i, j, k, s =
      match typ with
      | 3 ->
          let j = madd j 0
          and k = madd k 1
          and s = madd s 2 in
          (omsqrt j k s mask, j, k, s)
      | 4 ->
          let i = madd i 0
          and k = madd k 1
          and s = madd s 2 in
          (i, omsqrt i k s mask, k, s)
      | 5 ->
          let i = madd i 0
          and j = madd j 1
          and s = madd s 2 in
          (i, j, omsqrt i j s mask, s)
      | 6 ->
          let i = madd i 0
          and j = madd j 1
          and k = madd k 2 in
          (i, j, k, omsqrt i j k mask)
      | _ -> failwith "Me fail english? That's Umpossible!"
    in
    { Qtr.i = i; j = j; k = k; s = s }
  in
  Array.init posecount toquat
;;

let quat12 posecount sectbuf sbuf =
  let floats = Array.init 6 (fun i -> Xff.rfloat sbuf (i*4)) in
  let offset = Xff.rint sbuf 24 in
  let sbuf16 = Xff.sbufplus sectbuf offset in
  let madd v n =
    float v /. 32768.0 *. floats.(n*2 + 1) +. floats.(n*2)
  in
  let toquat poseno =
    let a = Xff.r16s sbuf16 (poseno*6 + 0)
    and b = Xff.r16s sbuf16 (poseno*6 + 2)
    and c = Xff.r16s sbuf16 (poseno*6 + 4) in
    let sign = a land 1 = 1 in
    let i = madd a 0
    and j = madd b 1
    and k = madd c 2 in
    let s = sqrt (1.0 -. (i*.i +. j*.j +. k*.k)) in
    Qtr.make i j k (if sign then -.s else s)
  in
  Array.init posecount toquat
;;

let quat13 posecount sectbuf sbuf =
  let bias   = Xff.rfloat sbuf 0
  and scale  = Xff.rfloat sbuf 4
  and x      = Xff.rfloat sbuf 8
  and y      = Xff.rfloat sbuf 12
  and z      = Xff.rfloat sbuf 16 in
  let offset = Xff.rint   sbuf 20 in
  let sbuf16 = Xff.sbufplus sectbuf offset in
  let toquat poseno =
    let fixp = Xff.r16s sbuf16 (poseno*2) in
    let fltp = float fixp /. 32768.0 in
    let phi = fltp*.scale +. bias in
    let q = Qtr.from_axis_angle x y z phi in
    { q with Qtr.s = q.Qtr.s }
  in
  Array.init posecount toquat
;;

let skin bones poseno =
  let quats = Array.map (fun a -> Array.get a poseno) bones in
  Skin.set_anim quats;
;;

let r xff sbufxff =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let sectpos = xff.Xff.sections.(1).Xff.off in
  let sectbuf = Xff.sbufplus sbufxff sectpos in
  let anbbuf = Xff.sbufplus sectbuf xff.Xff.entry in

  let bonecount = Xff.rint anbbuf 20 in
  let boneD =
    let off = Xff.rint anbbuf 16 in
    Array.init bonecount (fun i -> Xff.rint sectbuf (off + i*4))
  in
  let boneB =
    let off = Xff.rint anbbuf 8 in
    Array.init bonecount (fun i -> Xff.r8 sectbuf (off + i))
  in
  let posecount = 0xffff land Xff.rint anbbuf 4 in
  let rbone i =
    let d = boneD.(i)
    and b = boneB.(i) in
    let sbuf = Xff.sbufplus sectbuf d in
    match b with
    | 0             ->    quat0   posecount         sbuf
    | 3 | 4 | 5 | 6 -> quat3456 b posecount sectbuf sbuf
    | 12            ->   quat12   posecount sectbuf sbuf
    | 13            ->   quat13   posecount sectbuf sbuf
    | _             -> failwith "Me fail english? That's Umpossible!"
  in
  posecount, Array.init bonecount rbone
;;
