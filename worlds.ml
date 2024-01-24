(* World 1 *)
(* can probably meet Orbium *)
let cell () = if Random.float 1. < 0.775 then 0. else cell ()
let time  = 10.
let kernel_radius = 13
let gaussian_function x b c: float = exp (-.(((x -. b) /. c) ** 2. /. 2.))
let mu = 0.15
let sigma = 0.015
