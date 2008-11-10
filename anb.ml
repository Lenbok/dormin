open Format

let quat0 posecount sbuf =
  let i = Xff.rfloat sbuf 0
  and j = Xff.rfloat sbuf 4
  and k = Xff.rfloat sbuf 8
  and s = Xff.rfloat sbuf 12 in
  let q = Qtr.make i j k s in
  Array.create posecount q
;;

let quat3456 typ posecount sectbuf sbuf =
  let floats = Array.init 8 (fun i -> Xff.rfloat sbuf (i*4)) in
  let offset = Xff.rint sbuf 32 in
  let sbuf32 = Xff.sbufplus sectbuf offset in
  let int32s = Array.init posecount (fun i -> Xff.r32 sbuf32 (i*4)) in
  let toquat poseno =
    let cq = int32s.(poseno) in
    let omsqrt a b c bitno =
      let v = 1.0 -. (a*.a +. b*.b +. c*.c) in
      if v > 1e-9
      then
        let r = sqrt v in
        if Int32.to_int (Int32.shift_right_logical cq bitno) land 1 = 1
        then -.r
        else r
      else
        0.0
    in
    let nbf shift n =
      let i = 511 land Int32.to_int (Int32.shift_right_logical cq shift) in
      let i = i - ((i land 0b100000000) lsl 1) in
      let f = float i /. 256.0 in
      f*.floats.(n*2 + 1) +. floats.(n*2)
    in
    let i, j, k, s =
      match typ with
      | 3 ->
          let j = nbf 18 0
          and k = nbf  9 1
          and s = nbf  0 2 in
          (omsqrt j k s 31, j, k, s)
      | 4 ->
          let i = nbf 23 0
          and k = nbf  9 1
          and s = nbf  0 2 in
          (i, omsqrt i k s 22, k, s)
      | 5 ->
          let i = nbf 23 0
          and j = nbf 14 1
          and s = nbf  0 2 in
          (i, j, omsqrt i j s 13, s)
      | 6 ->
          let i = nbf 23 0
          and j = nbf 14 1
          and k = nbf  5 2 in
          (i, j, k, omsqrt i j k 4)
      | _ -> failwith "Me fail english? That's Umpossible!"
    in
    Qtr.make i j k s
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
    let m = 1.0 -. (i*.i +. j*.j +. k*.k) in
    let s = if m > 1e-9 then sqrt m else 0.0 in
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

let exact bones poseno =
  Array.map (fun a -> Array.get a poseno) bones
;;

let interpolated bones sposeno dposeno t =
  Array.init (Array.length bones)
    (fun i ->
      let sq = bones.(i).(sposeno)
      and dq = bones.(i).(dposeno) in
      Qtr.slerp sq dq t
    )
;;

let r xff sbufxff =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let sectpos = xff.Xff.sections.(1).Xff.off in
  let sectbuf = Xff.sbufplus sbufxff sectpos in
  let anbbuf = Xff.sbufplus sectbuf xff.Xff.entry in
  if not (Xff.cmp anbbuf (`dwords [12l]))
  then
    Xff.sbuferr anbbuf 0 "bad anb signature"
  ;

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

let append (p1, b1) (p2, b2) =
  let b = Array.init (Array.length b1) (fun i -> Array.append b1.(i) b2.(i)) in
  p1 + p2, b;
;;
