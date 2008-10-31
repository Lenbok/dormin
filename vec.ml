module IntOp =
struct
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let ( / ) = ( / )
end

module FltOp =
struct
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let ( / ) = ( /. )
end

type t = { x : float; y : float; z : float }

let vx v = v.x
let vy v = v.y
let vz v = v.z

let make x y z = { x = x; y = y; z = z }
let elts v = v.x, v.y, v.z

let origin = { x = 0.; y = 0.; z = 0. }
let x_axis = { origin with x = 1. }
let y_axis = { origin with y = 1. }
let z_axis = { origin with z = 1. }

open FltOp
let neg v = { x = -.v.x; y = -.v.y; z = -.v.z }
let add v u = { x = v.x + u.x; y = v.y + u.y; z = v.z + u.z }
let sub v u = { x = v.x - u.x; y = v.y - u.y; z = v.z - u.z }
let scale v s = { x = v.x * s; y = v.y * s; z = v.z * s }
let divide v s = { x = v.x / s; y = v.y / s; z = v.z / s }
let cross v u = { x = v.y * u.z - v.z * u.y;
                  y = v.z * u.x - v.x * u.z;
                  z = v.x * u.y - v.y * u.x }
let dot v u = v.x * u.x + v.y * u.y + v.z * u.z
let magnitude_squared v = dot v v
let magnitude v = sqrt (magnitude_squared v)
let normalize v = scale v (1. / (magnitude v))

let print ppf v =
  Format.fprintf ppf "[@[<1>x % 11f;@ y % 11f;@ z % 11f]@]" v.x v.y v.z

