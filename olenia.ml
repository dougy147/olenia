Random.self_init ()

let global_size = 128

let cell () = Random.float 1.
(*let cell () = if Random.float 1. < 0.775 then 0. else cell () (* Probability to "meet" Orbium *)*)
(*let size  = 64*)
let size  = global_size
let world = Array.init size (fun _ -> Array.init size (fun _ -> cell ()))

let time  = 10.
let kernel_radius = 13

let gaussian_function x b c: float = exp (-.(((x -. b) /. c) ** 2. /. 2.))
let mu = 0.15
let sigma = 0.015

let radius_vector = Array.init (2 * (kernel_radius)) (fun x -> float_of_int (x - (kernel_radius)) )

let access_2D_matrix (matrix: 'a array array) custom_function =
  let max_isize = (Array.length matrix) in
  let max_jsize = (Array.length matrix.(0)) in
  let rec aux maxi maxj i j =
    match i,j with
    | i,_  when i = maxi -> matrix
    | i, j when j = maxj -> aux maxi maxj (i+1) 0
    | i,j -> custom_function matrix i j ; aux maxi maxj i (j+1)
  in
  aux max_isize max_jsize 0 0

let build_distance (radius_vector: float array) (radius: int): float array array =
  let size = Array.length radius_vector in
  let d = Array.make_matrix size size 0. in
  let func mat i j = mat.(i).(j) <- ( (sqrt( (radius_vector.(i) ** 2.) +. (radius_vector.(j) ** 2.))) /. (float_of_int radius) ) in
  access_2D_matrix d func

let build_kernel (distance: float array array): float array array =
  let size = Array.length distance in
  let k = Array.make_matrix size size  0. in
  let kernel_mu = 0.5 in
  let kernel_sigma = 0.15 in
  let func mat i j = mat.(i).(j) <- ((distance.(i).(j)) *. (gaussian_function distance.(i).(j) kernel_mu kernel_sigma)) in
  access_2D_matrix k func

let reverse_signal_vertically (signal: float array array): float array array =
  let rec aux acc s =
    match s with
    | [] -> List.rev acc
    | hd :: tl -> aux ((List.rev hd)::acc) tl
  in
  aux [] (List.map (fun x -> Array.to_list x) (Array.to_list signal))
  |> List.map (fun x -> Array.of_list x)
  |> Array.of_list

let size (arr: 'a array) = Array.length arr

let convolute_kernel_over_world (world: float array array) (normalized_kernel: float array array): float array array =
  let kernel_horiz_center = ((size normalized_kernel - 1 ) / 2 ) in
  let kernel_verti_center = ((size normalized_kernel.(0) - 1 ) / 2 ) in
  let size_world = size world in

  let convolution = Array.make_matrix (size world) (size world) 0. in

  let toroid arr x y =
    arr.((x + size_world) mod size_world).((y + size_world) mod size_world)
  in

  let grab_neighbours arr x y =
    let neighbours = Array.make_matrix (2 * kernel_radius) (2 * kernel_radius) 0. in
    let rec aux imax jmax i j =
      match i,j with
      | i,_  when i = imax -> ()
      | i, j when j = jmax -> aux imax jmax (i+1) (-kernel_verti_center)
      | i,j -> neighbours.(i+kernel_horiz_center).(j+kernel_verti_center) <- toroid arr (x+i) (y+j) ; aux imax jmax i (j+1)
    in
    aux (kernel_horiz_center+1) (kernel_verti_center+1) (-kernel_horiz_center) (-kernel_verti_center);
    neighbours
  in

  let compute_neighbours_with_kernel neighbours kernel =
    let rec aux sum imax jmax i j =
      match i,j with
      | i,_ when i = imax -> sum
      | i,j when j = jmax -> aux sum imax jmax (i+1) 0
      | i,j -> aux (sum +. (neighbours.(i).(j) *. kernel.(i).(j))) imax jmax i (j+1)
    in
    aux 0. ((size neighbours) - 1) ((size neighbours) - 1) 0 0
  in

  let mult_by_kernel world kernel =
    let func mat i j = mat.(i).(j) <- compute_neighbours_with_kernel (grab_neighbours world i j) (kernel) in
    access_2D_matrix convolution func
  in
  mult_by_kernel world (reverse_signal_vertically normalized_kernel)
  (*mult_by_kernel world (normalized_kernel)*)

let normalize_kernel (kernel: float array array): float array array =
  let sum =
    let rec aux sum imax jmax i j =
      match i,j with
      | i,_ when i = imax -> sum
      | i,j when j = jmax -> aux sum imax jmax (i+1) 0
      | i,j -> aux (sum +. kernel.(i).(j)) imax jmax i (j+1)
    in
    aux 0. (size kernel) (size kernel) 0 0
  in
  let func mat i j = mat.(i).(j) <- (mat.(i).(j) /. sum) in
  access_2D_matrix kernel func

let apply_growth (world: float array array) (convolution: float array array): float array array =
  let func mat i j =
      let new_state = ( mat.(i).(j) +. ( ( 1. /. time ) *. ( ( (gaussian_function (convolution.(i).(j)) mu sigma) *. 2. ) -. 1.) ) ) in
      mat.(i).(j) <- if new_state < 0. then 0.
                       else
                         if new_state > 1. then 1.
                         else new_state;
  in
  access_2D_matrix world func

(* Construct the kernel :
   - distance from the center: *)
let d = build_distance radius_vector kernel_radius
(*
   - normalizing:  *)
let k = normalize_kernel (build_kernel d)

let next_world (world: float array array): float array array =
  apply_growth world (convolute_kernel_over_world world k)

;;

let title  = "OLenia - Lenia in OCaml"
let width  = 1024
let height = 1024
(*let grid   = 64*)
let grid   = global_size
let scaled_width  = width  / grid ;;
let scaled_height = height / grid ;;
let cell_width  = width  / scaled_width ;;
let cell_height = height / scaled_height ;;
let size_string = " " ^ (string_of_int (width-cell_width)) ^ "x" ^ (string_of_int (height-cell_height));;

type color = {r: float; g: float; b: float};;
(* Theme Borealis *)
let lower_color  = {r=20.; g=16.; b=110.};;
let higher_color = {r=0.; g=255.; b=14.};;

Graphics.open_graph size_string;;
Graphics.set_window_title title;;
(*Graphics.set_color Graphics.black;*)
Graphics.set_color (Graphics.rgb (int_of_float (lower_color.r)) (int_of_float (lower_color.g)) (int_of_float (lower_color.b)));;
Graphics.fill_rect 0 0 width height;; (* black background "hack" *)

(* let float_to_color f_num = int_of_float (f_num *. 255.) *)

let float_to_color f_num =
  (* Theme Borealis *)
  let lower_color  = {r=20.; g=16.; b=110.} in
  let higher_color = {r=0.; g=255.; b=14.} in
  let red   = (int_of_float (lower_color.r +. (f_num *. ( higher_color.r -. lower_color.r )))) in
  let green = (int_of_float (lower_color.g +. (f_num *. ( higher_color.g -. lower_color.g )))) in
  let blue  = (int_of_float (lower_color.b +. (f_num *. ( higher_color.b -. lower_color.b )))) in
  Graphics.rgb red green blue

let draw_point x y color =
  let c = float_to_color color in (* convert value in cell to a color *)
  Graphics.set_color c;
  Graphics.fill_circle ((x-1) * scaled_width) ((y-1) * scaled_height) (scaled_width/2);;
  (*
  Graphics.fill_rect ((x-1) * scaled_width) ((y-1) * scaled_height) scaled_width scaled_height;;
  *)

let print world =
  let max_isize = (Array.length world) - 1 in
  let max_jsize = (Array.length world.(0)) - 1 in
  let rec aux maxi maxj i j =
    match i,j with
    | i,_  when i = maxi -> ()
    | i, j when j = maxj -> aux maxi maxj (i+1) 0
    | i,j -> draw_point i j world.(i).(j) ; aux maxi maxj i (j+1)
  in
  aux max_isize max_jsize 0 0

let bigbang w =
  let rec aux generation w =
    let event = Graphics.wait_next_event [ Poll ] in
    if event.Graphics.keypressed then
      match (Graphics.read_key ()) with
      | '\027' -> Graphics.clear_graph();Graphics.close_graph()
      | 'r'    -> aux 0 (Array.init global_size (fun _ -> Array.init global_size (fun _ -> cell ())))
      | _      -> ()
    else
      print w;
      aux (generation+1) (next_world w)
  in
  aux 0 w

let () = bigbang world
