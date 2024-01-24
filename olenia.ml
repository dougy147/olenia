Random.self_init ()

let orbium =
  (*pattern["orbium"] = {"name":"Orbium","R":13,"T":10,"m":0.15,"s":0.015,"b":[1],*)
  [|
    [|0.;0.;0.;0.;0.;0.;0.1;0.14;0.1;0.;0.;0.03;0.03;0.;0.;0.3;0.;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.08;0.24;0.3;0.3;0.18;0.14;0.15;0.16;0.15;0.09;0.2;0.;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.15;0.34;0.44;0.46;0.38;0.18;0.14;0.11;0.13;0.19;0.18;0.45;0.;0.;0.|];
    [|0.;0.;0.;0.;0.06;0.13;0.39;0.5;0.5;0.37;0.06;0.;0.;0.;0.02;0.16;0.68;0.;0.;0.|];
    [|0.;0.;0.;0.11;0.17;0.17;0.33;0.4;0.38;0.28;0.14;0.;0.;0.;0.;0.;0.18;0.42;0.;0.|];
    [|0.;0.;0.09;0.18;0.13;0.06;0.08;0.26;0.32;0.32;0.27;0.;0.;0.;0.;0.;0.;0.82;0.;0.|];
    [|0.27;0.;0.16;0.12;0.;0.;0.;0.25;0.38;0.44;0.45;0.34;0.;0.;0.;0.;0.;0.22;0.17;0.|];
    [|0.;0.07;0.2;0.02;0.;0.;0.;0.31;0.48;0.57;0.6;0.57;0.;0.;0.;0.;0.;0.;0.49;0.|];
    [|0.;0.59;0.19;0.;0.;0.;0.;0.2;0.57;0.69;0.76;0.76;0.49;0.;0.;0.;0.;0.;0.36;0.|];
    [|0.;0.58;0.19;0.;0.;0.;0.;0.;0.67;0.83;0.9;0.92;0.87;0.12;0.;0.;0.;0.;0.22;0.07|];
    [|0.;0.;0.46;0.;0.;0.;0.;0.;0.7;0.93;1.;1.;1.;0.61;0.;0.;0.;0.;0.18;0.11|];
    [|0.;0.;0.82;0.;0.;0.;0.;0.;0.47;1.;1.;0.98;1.;0.96;0.27;0.;0.;0.;0.19;0.1|];
    [|0.;0.;0.46;0.;0.;0.;0.;0.;0.25;1.;1.;0.84;0.92;0.97;0.54;0.14;0.04;0.1;0.21;0.05|];
    [|0.;0.;0.;0.4;0.;0.;0.;0.;0.09;0.8;1.;0.82;0.8;0.85;0.63;0.31;0.18;0.19;0.2;0.01|];
    [|0.;0.;0.;0.36;0.1;0.;0.;0.;0.05;0.54;0.86;0.79;0.74;0.72;0.6;0.39;0.28;0.24;0.13;0.|];
    [|0.;0.;0.;0.01;0.3;0.07;0.;0.;0.08;0.36;0.64;0.7;0.64;0.6;0.51;0.39;0.29;0.19;0.04;0.|];
    [|0.;0.;0.;0.;0.1;0.24;0.14;0.1;0.15;0.29;0.45;0.53;0.52;0.46;0.4;0.31;0.21;0.08;0.;0.|];
    [|0.;0.;0.;0.;0.;0.08;0.21;0.21;0.22;0.29;0.36;0.39;0.37;0.33;0.26;0.18;0.09;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.;0.03;0.13;0.19;0.22;0.24;0.24;0.23;0.18;0.13;0.05;0.;0.;0.;0.|];
    [|0.;0.;0.;0.;0.;0.;0.;0.;0.02;0.06;0.08;0.09;0.07;0.05;0.01;0.;0.;0.;0.;0.|]
  |]

let global_size = 64

let cell () = Random.float 1.
(*let cell () = if Random.float 1. < 0.775 then 0. else cell () (* Probability to "meet" Orbium *)*)
let map_size  = global_size
let world = Array.init map_size (fun _ -> Array.init map_size (fun _ -> cell ()))

let rec insert_in_world (creature: float array array) (w: float array array) =
  for i = 0 to ((Array.length creature) - 1) do
    for j = 0 to ((Array.length creature.(i)) - 1) do
      w.(i).(j) <- creature.(i).(j)
    done
  done;
  w

(* Comment here if you don't want to load "orbium" on the map at startup *)
let world = Array.make_matrix map_size map_size 0.
let world = insert_in_world orbium world
(**)

let time  = 10.
let scale = 1
let kernel_radius = 13 * scale

let gaussian_function x b c: float = exp (-.(((x -. b) /. c) ** 2.) /. 2.)
let mu = 0.15
let sigma = 0.015

let map_middle = Array.init (map_size) (fun x -> float_of_int (x - (map_size / 2) - 1) )

let arrsize (arr: 'a array) = Array.length arr

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

let build_distance (map_middle: float array) (radius: int): float array array =
  let s = arrsize map_middle in
  let d = Array.make_matrix (s) (s) 0. in
  let func mat i j = mat.(i).(j) <- ( (sqrt( (map_middle.(i) ** 2.) +. (map_middle.(j) ** 2.))) /. (float_of_int radius) ) in
  access_2D_matrix d func

let build_kernel (distance: float array array): float array array =
  let s = arrsize distance in
  let k = Array.make_matrix s s  0. in
  let kernel_mu = 0.5 in
  let kernel_sigma = 0.15 in
  let func mat i j = mat.(i).(j) <- if distance.(i).(j) < 1. then (1. *. (gaussian_function distance.(i).(j) kernel_mu kernel_sigma)) else 0. in
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

(* Fourier Transform related functions *)
let fft_shift (matrix: 'a array array): 'a array array =
  (*Cuts in half horizontally and swaps, then cuts vertically and swaps*)
  let bit = if (arrsize matrix) mod 2 = 0 then 0 else 1 in
  let half_horiz_size = ((arrsize matrix) / 2) + bit in
  let half_verti_size = ((arrsize matrix.(0)) / 2) + bit in
  let cut_and_swap rows_or_cols s = Array.append (Array.sub rows_or_cols (s+1) ((arrsize rows_or_cols) - (s+1))) (Array.sub rows_or_cols 0 (s+1)) in
  let horiswap = cut_and_swap matrix half_horiz_size in
  (* TODO : weird way of iterating through rows of array *)
  let rec iterate_rows i =
    match i with
    | _ when i = arrsize matrix -> horiswap
    | _ ->
      horiswap.(i) <- (cut_and_swap horiswap.(i) half_verti_size);
      iterate_rows (i+1)
  in
  iterate_rows 0

let mult_float_complex (x: float) (c: Complex.t): Complex.t =
  Complex.{re=x*.c.re; im=x*.c.im}

let div_complex_float (c: Complex.t) (x: float): Complex.t =
  Complex.{re=c.re/.x; im=c.im/.x}

let imaginary = Complex.{re=0.;im=1.}

(* Discrete Fourier Transform (slow but used when array is not of a power of two length *)
let dft (v: float array): Complex.t array =
  let n = arrsize v in
  let y = Array.init (n) (fun x -> Complex.{re=0.;im=0.}) in
  let func k n' = Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi *. (float_of_int k) *. (float_of_int n')) (imaginary)) (float_of_int n))
  in
  for k = 0 to (n-1) do
    let s = ref Complex.{re=0.;im=0.} in
    for n' = 0 to (n-1) do
      s := Complex.add !s (mult_float_complex (v.(n')) (func k n'))
    done;
    y.(k) <- !s
  done;
  y

let dft_complex (v: Complex.t array): Complex.t array =
  let n = arrsize v in
  let y = Array.init (n) (fun x -> Complex.{re=0.;im=0.}) in
  let func k n' = Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi *. (float_of_int k) *. (float_of_int n')) (imaginary)) (float_of_int n))
  in
  for k = 0 to (n-1) do
    let s = ref Complex.{re=0.;im=0.} in
    for n' = 0 to (n-1) do
      s := Complex.add !s (Complex.mul (v.(n')) (func k n'))
    done;
    y.(k) <- !s
  done;
  y

let rec fft_1D (v: float array): Complex.t array =
  let n = arrsize v in
  if n = 1 then
    [|Complex.{re=v.(0);im=0.}|]
  else
    if (mod_float (Float.log2 (float_of_int n)) 2.) != mod_float (Float.log2 (float_of_int n)) 2. then
      dft v
    else
    let even_part = Array.init (n/2) (fun x -> v.(2*x)    )
    and odd_part  = Array.init (n/2) (fun x -> v.(2*x + 1))
    in
    let ye = fft_1D even_part
    and yo = fft_1D odd_part in
    let y = Array.init (n) (fun x -> Complex.{re=0.;im=0.}) in
    for index = 0 to ((n/2)-1) do
      y.(index)         <- Complex.add (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (2. *. Float.pi) (imaginary)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ));
      y.(index + (n/2)) <- Complex.sub (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (2. *. Float.pi) (imaginary)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ))
    done;
    y

let rec fft_1D_complex (v: Complex.t array): Complex.t array =
  let n = arrsize v in
  if n = 1 then
    v
  else
    if (mod_float (Float.log2 (float_of_int n)) 2.) != mod_float (Float.log2 (float_of_int n)) 2. then
      dft_complex v
    else
    let even_part = Array.init (n/2) (fun x -> v.(2*x)    )
    and odd_part  = Array.init (n/2) (fun x -> v.(2*x + 1))
    in
    let ye = fft_1D_complex even_part
    and yo = fft_1D_complex odd_part in
    let y = Array.init (n) (fun x -> Complex.{re=0.;im=0.}) in
    for index = 0 to ((n/2)-1) do
      y.(index)         <- Complex.add (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (2. *. Float.pi) (imaginary)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ));
      y.(index + (n/2)) <- Complex.sub (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (2. *. Float.pi) (imaginary)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ))
    done;
    y

let rec ifft_1D (v: Complex.t array): Complex.t array =
  let n = arrsize v in
  let y = Array.init (n) (fun x -> Complex.{re=0.;im=0.}) in
  let w n' = Complex.exp (div_complex_float (mult_float_complex (-2. *. Float.pi) (imaginary)) (float_of_int n))
  in
  for k = 0 to (n-1) do
    let s = ref Complex.{re=0.;im=0.} in
    for n' = 0 to (n-1) do
      s := Complex.add !s (Complex.mul (v.(n')) (Complex.pow (w n') Complex.{re=float_of_int (-k * n');im=0.}))
    done;
    y.(k) <- div_complex_float !s (float_of_int n)
    (*y.(k) <- Complex.div !s Complex.{re=float_of_int n;im=0.}*)
  done;
  y

let fft2 matrix =
  let sr = arrsize matrix in
  let sc = arrsize (matrix.(0)) in
  (* start with cols *)
  let fft_col = Array.init (sc) (fun x -> [||]) in
  for c = 0 to (sc - 1) do
    let col = Array.init (sc) (fun x -> 0.) in
    for r = 0 to (sr - 1) do
      col.(r) <- matrix.(r).(c);
    done;
    fft_col.(c) <- fft_1D col;
  done;
  (* rotate back *)
  let fft_col_applied = Array.make_matrix sc sr Complex.{re=0.;im=0.} in
  for r = 0 to (sr - 1) do
    for c = 0 to (sc - 1) do
      fft_col_applied.(c).(r) <- fft_col.(r).(c)
    done
  done;
  (* apply to rows *)
  let fft = Array.make_matrix sr sc Complex.{re=0.;im=0.} in
  for r = 0 to (sr - 1) do
    fft.(r) <- fft_1D_complex (fft_col_applied.(r))
  done;
  fft

let ifft2 matrix =
  let sr = arrsize matrix in
  let sc = arrsize (matrix.(0)) in
  (* start with cols *)
  let fft_col = Array.init (sc) (fun x -> [||]) in
  for c = 0 to (sc - 1) do
    let col = Array.init (sc) (fun x -> Complex.{re=0.;im=0.}) in
    for r = 0 to (sr - 1) do
      col.(r) <- matrix.(r).(c);
    done;
    fft_col.(c) <- ifft_1D col;
  done;
  (* rotate back *)
  let fft_col_applied = Array.make_matrix sc sr Complex.{re=0.;im=0.} in
  for r = 0 to (sr - 1) do
    for c = 0 to (sc - 1) do
      fft_col_applied.(c).(r) <- fft_col.(r).(c)
    done
  done;
  (* apply to rows *)
  (*let fft = Array.make_matrix sr sc Complex.{re=0.;im=0.} in*)
  let fft = Array.init sr (fun x -> [||]) in
  for r = 0 to (sr - 1) do
    fft.(r) <- (ifft_1D (fft_col_applied.(r)))
  done;
  fft

let complex_to_real_matrix (matrix: Complex.t array array): float array array =
  let sr = arrsize matrix in
  let sc = (arrsize matrix.(0)) in
  let mat2 = Array.make_matrix sr sc 0. in
  for i = 0 to (sr-1) do
    for j = 0 to (sc-1) do
      mat2.(i).(j) <- matrix.(i).(j).re
    done
  done;
  mat2

let mult_complex_matrices (mat1: Complex.t array array) (mat2: Complex.t array array): Complex.t array array =
  let s = arrsize mat1 in (* matrices will always be of same size so ...*)
  let mat3 = Array.make_matrix s s Complex.{re=0.;im=0.} in
  for i = 0 to (s-1) do
    for j = 0 to (s-1) do
      mat3.(i).(j) <- Complex.mul mat1.(i).(j) mat2.(i).(j)
    done
  done;
  mat3

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
  let u = complex_to_real_matrix (ifft2 (mult_complex_matrices (fft2 (world)) fk ) )  in
  apply_growth world u
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
      aux (generation+1) (next_world w)
  in
  aux 0 w

let () = bigbang world
