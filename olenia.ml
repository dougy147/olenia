Random.self_init ()

(* Fourier functions can receive both float and complex arrays as parameters.
 * The type "number" correspond to those types.
 *)
type number =
 | Float of float
 | Complex of Complex.t

let float_value = function
 | Float f -> f
 | _ -> raise Not_found

let global_size = 64

let cell () = Random.float 1.
(*let cell () = if Random.float 1. < 0.775 then 0. else cell () (* Probability to "meet" Orbium *)*)
let map_size  = global_size
let world = Array.init map_size (fun _ -> Array.init map_size (fun _ -> cell ()))

(* Comment here if you don't want to load a creature on the map at startup *)
(*
let creature = (* insert array for creature here *)
let rec insert_in_world (creature: float array array) (w: float array array) =
  for i = 0 to ((Array.length creature) - 1) do
    for j = 0 to ((Array.length creature.(i)) - 1) do
      w.(i).(j) <- creature.(i).(j)
    done
  done;
  w
let world = Array.make_matrix map_size map_size 0.
let world = insert_in_world creature world
*)

let time  = 10.
let scale = 1
let kernel_radius = 13 * scale

let gaussian_function x b c: float = exp (-.(((x -. b) /. c) ** 2.) /. 2.)
let mu = 0.135
let sigma = 0.015

let map_middle =
  if map_size mod 2 = 0 then
    Array.init (map_size) (fun x -> float_of_int (x - (map_size / 2) - 1) )
  else
    Array.init (map_size) (fun x -> float_of_int (x - (map_size / 2) - 2 ) )

let arrsize (arr: 'a array) = Array.length arr

let access_2D_matrix (matrix: 'a array array) custom_function =
  let sr = arrsize matrix in
  let sc = arrsize matrix.(0) in
  Array.init sr (fun r -> Array.init sc (fun c -> custom_function matrix r c ))
  (*
  let rec aux maxi maxj i j =
    match i,j with
    | i,_  when i = maxi -> matrix
    | i, j when j = maxj -> aux maxi maxj (i+1) 0
    | i,j -> matrix.(i).(j) <- custom_function matrix i j ; aux maxi maxj i (j+1)
  in
  aux sr sc 0 0
  *)

let build_distance (map_middle: float array) (radius: int): float array array =
  let s = arrsize map_middle in
  let d = Array.make_matrix (s) (s) 0. in
  let func mat i j = ( (sqrt( (map_middle.(i) ** 2.) +. (map_middle.(j) ** 2.))) /. (float_of_int radius) ) in
  access_2D_matrix d func

let build_kernel (distance: float array array): float array array =
  let s = arrsize distance in
  let k = Array.make_matrix s s  0. in
  let kernel_mu = 0.5 in
  let kernel_sigma = 0.15 in
  (*let b = [|0.5;1.;2./.3.|] in*)
  (*
  let func mat i j =
      if distance.(i).(j) < Float.of_int (arrsize b) then
        (( b.(min (int_of_float (distance.(i).(j))) ((arrsize b) - 1))) *. (gaussian_function distance.(i).(j) kernel_mu kernel_sigma))
      else
        0.
  in
  *)
  let func mat i j = if distance.(i).(j) < 1. then (1. *. (gaussian_function distance.(i).(j) kernel_mu kernel_sigma)) else 0. in
  access_2D_matrix k func

let normalize_kernel (kernel: float array array): float array array =
  let sum =
    let rec aux sum imax jmax i j =
      match i,j with
      | i,_ when i = imax -> sum
      | i,j when j = jmax -> aux sum imax jmax (i+1) 0
      | i,j -> aux (sum +. kernel.(i).(j)) imax jmax i (j+1)
    in
    aux 0. (arrsize kernel) (arrsize kernel) 0 0
  in
  let func mat i j = (mat.(i).(j) /. sum) in
  access_2D_matrix kernel func

let apply_growth (world: float array array) (convolution: float array array): float array array =
  let func mat i j =
      let new_state = ( mat.(i).(j) +. ( ( 1. /. time ) *. ( ( (gaussian_function (convolution.(i).(j)) mu sigma) *. 2. ) -. 1.) ) ) in
      if new_state < 0. then 0.
                       else
                         if new_state > 1. then 1.
                         else new_state;
  in
  access_2D_matrix world func

(* Fourier Transform related functions *)
let fft_shift (matrix: 'a array array): 'a array array =
  (*Cuts in half horizontally and swaps, then cuts vertically and swaps*)
  let bit = if (arrsize matrix) mod 2 = 0 then 0 else 1 in
  let half_horiz_size = ((arrsize matrix) / 2) + bit in
  let half_verti_size = ((arrsize matrix.(0)) / 2) + bit in
  let cut_and_swap rows_or_cols s = Array.append (Array.sub rows_or_cols (s+1) ((arrsize rows_or_cols) - (s+1))) (Array.sub rows_or_cols 0 (s+1)) in
  let horiswap = cut_and_swap matrix half_horiz_size in
  let rec iterate_rows i =
    match i with
    | _ when i = arrsize matrix -> horiswap
    | _ ->
      horiswap.(i) <- (cut_and_swap horiswap.(i) half_verti_size);
      iterate_rows (i+1)
  in
  iterate_rows 0

(* Operations between floats and complex numbers *)
let mult_float_complex (x: float) (c: Complex.t): Complex.t =
  Complex.{re=x*.c.re; im=x*.c.im}

let div_complex_float (c: Complex.t) (x: float): Complex.t =
  Complex.{re=c.re/.x; im=c.im/.x}

(* Discrete Fourier Transform (slow but used when array is not of a power of two length *)
let dft (v: number array): Complex.t array =
  Printf.printf "Using DFT (slow)";
  let n = arrsize v in
  let func k n' = Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi *. (float_of_int k) *. (float_of_int n')) (Complex.i)) (float_of_int n))
  in
  let accu = Complex.zero in
  let f acc k n' = match v.(n') with
    | Float   x -> Complex.add acc (mult_float_complex (x) (func k n'))
    | Complex x -> Complex.add acc (Complex.mul (x) (func k n'))
  in
  let v_indexes = (Array.init (n) (fun x -> x)) in
  Array.map (fun k -> Array.fold_left (fun acc n' -> f acc k n') (accu) (v_indexes)) v_indexes

let rec fft_1D (v: number array): Complex.t array =
  let n = arrsize v in
  if n = 1 then
    match v.(0) with
    | Float   x -> [|Complex.{re=x;im=0.}|]
    | Complex x -> [|x|]
  else
    if not ( Float.of_int (int_of_float ((mod_float (Float.log2 (float_of_int n)) 2.))) = mod_float (Float.log2 (float_of_int n)) 2.) then
    match v.(0) with
    | Float   x -> dft (Array.map (fun y -> Complex Complex.{re=float_value y;im=0.}) v)
    | Complex x -> dft v
    else
    let even_part = Array.init (n/2) (fun x -> v.(2*x)    )
    and odd_part  = Array.init (n/2) (fun x -> v.(2*x + 1))
    in
    let ye = fft_1D even_part
    and yo = fft_1D odd_part in
    let add index =
      Complex.add (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi) (Complex.i)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ))
    in
    let sub index =
      Complex.sub (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi) (Complex.i)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ))
    in
    let v_indexes = (Array.init (n/2) (fun x -> x)) in
    Array.append (Array.map (fun i -> add i) v_indexes) (Array.map (fun i -> sub i) v_indexes)

let rec ifft_1D (v: Complex.t array): Complex.t array =
  let n = arrsize v in
  let w = Complex.exp (div_complex_float (mult_float_complex (-2. *. Float.pi) (Complex.i)) (float_of_int n)) in
  let accu = Complex.zero in
  let f acc k n' = Complex.add (acc) (Complex.mul (v.(n')) (Complex.pow (w) Complex.{re=float_of_int (-k * n');im=0.})) in
  let v_indexes = Array.mapi (fun x -> fun i -> x) v in
  Array.map (fun k -> div_complex_float (Array.fold_left (fun acc n' -> f acc k n') (accu) (v_indexes)) (float_of_int n) ) v_indexes

let fft2 (matrix: float array array) =
  let sr = arrsize matrix in
  let sc = arrsize matrix.(0) in
  (* 1. Apply fft_1D to columns (by transposing them as rows) *)
  let fft_col = Array.init sc (fun c -> fft_1D (Array.init sr (fun r -> Float matrix.(r).(c)))) in
  (* 2. Transpose back and apply fft_1D to rows *)
  Array.init sr (fun r -> fft_1D (Array.init sc (fun c -> Complex fft_col.(c).(r))))

let ifft2 matrix =
  let sr = arrsize matrix in
  let sc = arrsize matrix.(0) in
  (* 1. Apply ifft_1D to columns (by transposing them as rows) *)
  let fft_col = Array.init sc (fun c -> ifft_1D (Array.init sr (fun r -> matrix.(r).(c)))) in
  (* 2. Transpose back and apply ifft_1D to rows *)
  Array.init sr (fun r -> ifft_1D (Array.init sc (fun c -> fft_col.(c).(r))))

let complex_to_real_matrix (matrix: Complex.t array array): float array array =
  (* we can do pattern matching with parameters of lambda function! *)
  Array.map (fun row -> Array.map (fun Complex.{re=elem;im=_} -> elem) row) matrix

let mult_complex_matrices (mat1: Complex.t array array) (mat2: Complex.t array array): Complex.t array array =
  Array.map2 (fun row1 row2 -> Array.map2 (fun elem1 elem2 -> Complex.mul elem1 elem2) row1 row2 ) mat1 mat2

(* Construct the kernel :
   - distance from the center: *)
let d = build_distance map_middle kernel_radius
(*
   - normalizing:  *)
let k = normalize_kernel (build_kernel d)

(* Pre-computed FFT of the kernel *)
(*let fk = fft2 (k)*) (* observed stable structures here *)
let fk = fft2 (fft_shift k)

let next_world (world: float array array): float array array =
  (*let u = complex_to_real_matrix (ifft2 (mult_complex_matrices (fft2 (world)) fk ) )  in
  apply_growth world u
  *)
  apply_growth world (complex_to_real_matrix (ifft2 (mult_complex_matrices (fft2 (world)) fk ) ))
;;

let title  = "OLenia - Lenia in OCaml"
let width  = 1024
let height = 1024
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

(*Set background color*)
Graphics.set_color (Graphics.rgb (int_of_float (lower_color.r)) (int_of_float (lower_color.g)) (int_of_float (lower_color.b)));;
Graphics.fill_rect 0 0 width height;;

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
  (* If you prefer to draw squares instead of circles
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
      (*Printf.printf "%d\n" generation;*)
      aux (generation+1) (next_world w)
  in
  aux 0 w

let () = bigbang world
