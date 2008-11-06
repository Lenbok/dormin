(* A lot of this was taken from different sources
   Including NVidias examples
   Mikael Kalms' code
   Wikipedia
*)

open Vec

type t = { i : float; j : float; k : float; s : float }

open FltOp

let id = { i = 0.; j = 0.; k = 0.; s = 1.0 }
let conjugate q = { i = -.q.i; j = -.q.j; k = -.q.k; s = q.s }
let magnitude_squared q = q.i * q.i + q.j * q.j + q.k * q.k + q.s * q.s
let magnitude q = sqrt (magnitude_squared q)
let scale q s = { i = q.i * s; j = q.j * s; k = q.k * s; s = q.s * s }
let norm q = let s = 1.0 / magnitude q in scale q s
let add q p = { i = q.i + p.i; j = q.j + p.j; k = q.k + p.k; s = q.s + p.s }
let dot q p = q.i * p.i + q.j * p.j + q.k * p.k + q.s * p.s
let elems q = q.i, q.j, q.k, q.s
let make i j k s = { i = i; j = j; k = k; s = s }

let compose q p =
  let s = q.s * p.s - (q.i * p.i + q.j * p.j + q.k * p.k)
  and i = q.s * p.i + p.s * q.i + q.j * p.k - q.k * p.j
  and j = q.s * p.j + p.s * q.j + q.k * p.i - q.i * p.k
  and k = q.s * p.k + p.s * q.k + q.i * p.j - q.j * p.i in
  { i = i; j = j; k = k; s = s }

let from_euler yaw pitch roll =
  let half_yaw = yaw / 2.0
  and half_pitch = pitch / 2.0
  and half_roll = roll / 2.0 in

  let cosYaw = cos half_yaw
  and cosRoll = cos half_roll
  and cosPitch = cos half_pitch
  and sinYaw = sin half_yaw
  and sinRoll = sin half_roll
  and sinPitch = sin half_pitch in

  { i = sinRoll * cosPitch * cosYaw - cosRoll * sinPitch * sinYaw;
    j = cosRoll * sinPitch * cosYaw + sinRoll * cosPitch * sinYaw;
    k = cosRoll * cosPitch * sinYaw - sinRoll * sinPitch * cosYaw;
    s = cosRoll * cosPitch * cosYaw + sinRoll * sinPitch * sinYaw }

let from_axis_angle x y z phi =
  let phi = phi * 0.5 in
  let mag = sqrt (x * x + y * y + z * z) in
  let s = sin phi / mag in
  let x = x * s and y = y * s and z = z * s in
  { i = x; j = y; k = z; s = cos phi }

let to_norm_axis_angle q =
  let magsq = q.i * q.i + q.j * q.j + q.k * q.k in
  if magsq > 0.0 then
    let theta = 2.0 * acos q.s in
    let s = 1.0 / sqrt magsq in
    { z = q.i * s; y = q.j * s; x = q.k * s }, theta
  else x_axis, 0.0

let to_axis_angle q = { z = q.i; y = q.j; x = q.k }, 2.0 * acos q.s

let slerp q p t =
  let cosphi = dot q p in
  let cosphi, flip = if cosphi < 0.0 then -.cosphi, true else cosphi, false in
  if 1.0 - cosphi < 1e-8 then
    q
  else
    let phi = acos cosphi in
    let sphi = 1.0 / sin phi in
    let k0 = sin ((1.0 - t) * phi) * sphi
    and k1 = sin (t * phi) * sphi in
    let k1 = if flip then -. k1 else k1 in
    let q' = scale q k0 in
    let p' = scale p k1 in
    add q' p'

let apply q v =
  let a = -.q.s in
  let b = q.i in
  let c = q.j in
  let d = q.k in
  let v1 = v.Vec.x in
  let v2 = v.Vec.y in
  let v3 = v.Vec.z in
  let t2 =   a*.b in
  let t3 =   a*.c in
  let t4 =   a*.d in
  let t5 =  -.b*.b in
  let t6 =   b*.c in
  let t7 =   b*.d in
  let t8 =  -.c*.c in
  let t9 =   c*.d in
  let t10 = -.d*.d in
  { Vec.x = 2.*.( (t8 +. t10)*.v1 +. (t6 -.  t4)*.v2 +. (t3 +. t7)*.v3 ) +. v1
  ;     y = 2.*.( (t4 +.  t6)*.v1 +. (t5 +. t10)*.v2 +. (t9 -. t2)*.v3 ) +. v2
  ;     z = 2.*.( (t7 -.  t3)*.v1 +. (t2 +.  t9)*.v2 +. (t5 +. t8)*.v3 ) +. v3
  }

let print ppf q =
  Format.fprintf ppf "[@[<2>i % f;@ j % f;@ k % f;@ s % f@]]" q.i q.j q.k q.s
