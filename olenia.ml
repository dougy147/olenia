Random.self_init ()

let cell () = Random.float 1.
(*let cell () = if Random.float 1. < 0.775 then 0. else cell ()*)

(* Parameters initialization (command-line arguments parsing if any) *)
let usage_msg = "olenia [-s int] [-w int] [-h int] [-mu float] [-sd float] [-kmu float] [-ksd float] [-radius int] [-time float] [-scale int] [-shape <ellipse|square>] [-theme <borealis|redsoft|tournesol>]"

let time          = ref 10.
let scale         = ref 1
let kernel_radius = ref 13
let convo_mu      = ref 0.15
let convo_sigma   = ref 0.015
let kernel_mu     = ref 0.5
let kernel_sigma  = ref 0.15
let map_size      = ref 64
let width         = ref 800
let height        = ref 800
let shape         = ref "ellipse"
let theme         = ref "borealis"

let speclist =
  [
    ("-s", Arg.Set_int map_size, ": map size (int)");
    ("-w", Arg.Set_int width, ": window width (int)");
    ("-h", Arg.Set_int height, ": window height (int)");
    ("-time", Arg.Set_float time, ": time discretization (float)");
    ("-mu", Arg.Set_float convo_mu, ": convolution gaussian mean (float)");
    ("-sd", Arg.Set_float convo_sigma, ": convolution gaussian standard deviation (float)");
    ("-kmu", Arg.Set_float kernel_mu, ": kernel gaussian mean (float)");
    ("-ksd", Arg.Set_float kernel_sigma, ": kernel gaussian standard deviation (float)");
    ("-radius", Arg.Set_int kernel_radius, ": kernel radius (int)");
    ("-scale", Arg.Set_int scale, ": zoom level (int)");
    ("-shape", Arg.Set_string shape, ": cells shape (ellipse or square)");
    ("-theme", Arg.Set_string theme, ": color gradient (borealis, redsoft or tournesol)");
  ]

let parse_arguments = Arg.parse speclist (fun _ -> ()) usage_msg;;
parse_arguments
;;

let draw_shape x y sw sh =
  match !shape with
  | "ellipse" -> Graphics.fill_ellipse (x * sw) (y * sh) (sw/2) (sh/2)
  | "square"  -> Graphics.fill_rect (x * sw) (y * sh) (sw) (sh)
  | _ -> failwith "This shape does not exist."

let half_size = !map_size / 2
(* End of parameters initialization *)

let world: float array array = Array.init !map_size (fun _ -> Array.init !map_size (fun _ -> cell ()))

let initialize_world () =
  Array.iteri (fun i r -> Array.iteri (fun j c -> world.(i).(j) <- cell () ) r) world

let gaussian_function x mu sd: float = exp (-. ( ( (x -. mu) /. sd) ** 2.) /. 2.)

let build_distance (radius: int) = (* distance from the center of the kernel *)
  let map_middle = Array.init (!map_size) (fun x -> float_of_int (x - (!map_size / 2) - (if !map_size mod 2 = 0 then 1 else 2) ) ) in
  let s = Array.length map_middle in
  let d = Array.make_matrix (s) (s) 0. in
  Array.iteri (fun i r -> Array.iteri (fun j c -> d.(i).(j) <- ( (sqrt( (map_middle.(i) ** 2.) +. (map_middle.(j) ** 2.))) /. (float_of_int radius) )  ) r) d;
  d

let build_kernel (distance: float array array) =
  let s = Array.length distance in
  let k = Array.make_matrix s s  0. in
  let normalize_kernel (kernel: float array array) =
    let sum = List.fold_left (fun acc x -> x +. acc ) 0. (Array.to_list (Array.map (fun x -> Array.to_list x) kernel) |> List.flatten) in
    Array.iteri (fun i r -> Array.iteri (fun j c -> kernel.(i).(j) <- (kernel.(i).(j) /. sum) ) r) kernel;
    kernel
  in
  Array.iteri (fun i r -> Array.iteri (fun j c -> k.(i).(j) <- if distance.(i).(j) < 1. then (1. *. (gaussian_function distance.(i).(j) !kernel_mu !kernel_sigma)) else 0. ) r) distance;
  normalize_kernel k

let apply_growth (convolution: float array array) =
  let f i j c = let ns = ( c +. ( ( 1. /. !time ) *. ( ( (gaussian_function (convolution.(i).(j)) !convo_mu !convo_sigma) *. 2. ) -. 1.) ) ) in
    if ns < 0. then 0.
    else
      if ns > 1. then 1.
      else ns
    in
  Array.iteri (fun i r -> Array.iteri ( fun j c -> world.(i).(j) <- f i j c ) r) world

(* Operations between floats and complex numbers or latter only *)
let mult_float_complex (x: float) (c: Complex.t): Complex.t =
  Complex.{re=x*.c.re; im=x*.c.im}

let div_complex_float (c: Complex.t) (x: float): Complex.t =
  Complex.{re=c.re/.x; im=c.im/.x}

let complex_to_real_matrix (matrix: Complex.t array array) =
  Array.map (fun row -> Array.map (fun Complex.{re=elem;im=_} -> elem) row) matrix

let mult_complex_matrices (mat1: Complex.t array array) (mat2: Complex.t array array): Complex.t array array =
  Array.map2 (fun r1 r2 -> Array.map2 (Complex.mul) r1 r2 ) mat2 mat1

(* Fourier Transform related functions *)
let fft_shift (matrix: 'a array array): 'a array array =
  (*Cuts in half horizontally and swaps, then cuts vertically and swaps*)
  let bit = if (Array.length matrix) mod 2 = 0 then 0 else 1 in
  let half_horiz_size = ((Array.length matrix) / 2) + bit in
  let half_verti_size = ((Array.length matrix.(0)) / 2) + bit in
  let cut_and_swap rows_or_cols s = Array.append (Array.sub rows_or_cols (s+1) ((Array.length rows_or_cols) - (s+1))) (Array.sub rows_or_cols 0 (s+1)) in
  let horiswap = cut_and_swap matrix half_horiz_size in
  let rec iterate_rows i =
    match i with
    | _ when i = Array.length matrix -> horiswap
    | _ ->
      horiswap.(i) <- (cut_and_swap horiswap.(i) half_verti_size);
      iterate_rows (i+1)
  in
  iterate_rows 0

(* Discrete Fourier Transform (slow but used when array is not of a power of two length *)
let dft (v: Complex.t array): Complex.t array =
  let n = Array.length v in
  let func k n' = Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi *. (float_of_int k) *. (float_of_int n')) (Complex.i)) (float_of_int n)) in
  let f acc k n' = Complex.add acc (Complex.mul (v.(n')) (func k n')) in
  let v_indexes = (Array.init (n) (fun x -> x)) in
  Array.map (fun k -> Array.fold_left (fun acc n' -> f acc k n') (Complex.zero) (v_indexes)) v_indexes

let rec fft_1D (v: Complex.t array) =
  let n = Array.length v in
  if n = 1 then
    v
  else
    if not ( Float.of_int (int_of_float ((mod_float (Float.log2 (float_of_int n)) 2.))) = mod_float (Float.log2 (float_of_int n)) 2.) then
      dft v (* If the size of the map is not of size power of 2, then use DFT *)
    else
      let even_part = Array.init (n/2) (fun x -> v.(2*x))
      and odd_part  = Array.init (n/2) (fun x -> v.(2*x + 1)) in
      let ye = fft_1D even_part
      and yo = fft_1D odd_part in
      let func op index =
        op (ye.(index)) (Complex.mul (yo.(index)) (Complex.pow (Complex.exp (div_complex_float (mult_float_complex (-. 2. *. Float.pi) (Complex.i)) (float_of_int n) )) (Complex.{re=float_of_int index;im=0.}) ))
      in
      let v_indexes = (Array.init (n/2) (fun x -> x)) in
      Array.iter (fun i -> v.(i) <- func (Complex.add) i ) v_indexes;
      Array.iter (fun i -> v.(i+n/2) <- func (Complex.sub) i ) v_indexes;
      v

let rec ifft_1D (v: Complex.t array): Complex.t array =
  let n = Array.length v in
  let w = Complex.exp (div_complex_float (mult_float_complex (-2. *. Float.pi) (Complex.i)) (float_of_int n)) in
  let v_indexes = Array.init (n) (fun x -> x) in
  (*let v_indexes = Array.mapi (fun x -> fun i -> x) v in*)
  let f acc k n' = Complex.add (acc) (Complex.mul (v.(n')) (Complex.pow (w) Complex.{re=float_of_int (-k * n');im=0.})) in
  Array.map (fun k -> div_complex_float (Array.fold_left (fun acc n' -> f acc k n') (Complex.zero) (v_indexes)) (float_of_int n) ) v_indexes

let fft_2D (matrix: float array array) =
  let fft_col = Array.init !map_size (fun c -> fft_1D (Array.init !map_size (fun r -> Complex.{re=matrix.(r).(c);im=0.}))) in
  Array.init !map_size (fun r -> fft_1D (Array.init !map_size (fun c -> fft_col.(c).(r))))

let ifft_2D (matrix: Complex.t array array) =
  let ifft_col = Array.init !map_size (fun c -> ifft_1D (Array.init !map_size (fun r -> matrix.(r).(c)))) in
  Array.init !map_size (fun r -> ifft_1D (Array.init !map_size (fun c -> ifft_col.(c).(r))))


(* Construct kernel *)
let k  = build_kernel (build_distance (!kernel_radius * !scale))
let fk = fft_2D (fft_shift k)

let next_world (world: float array array) =
  world
    |> fft_2D
    |> mult_complex_matrices fk
    |> ifft_2D
    |> complex_to_real_matrix
    |> apply_growth
;;


let title  = "OLenia"
(*
let width  = 800
let height = 800
*)
let grid   = !map_size
let scaled_width  = !width  / grid
let rest_width    = !width  - (grid * scaled_width)
let cell_width    = !width  / scaled_width
let scaled_height = !height / grid
let rest_height   = !height - (grid * scaled_height)
let cell_height   = !height / scaled_height
let size_string   = " " ^ (string_of_int (scaled_width * cell_width - rest_width)) ^ "x" ^ (string_of_int (scaled_height * cell_height - rest_height))

type color = {r: float; g: float; b: float}

let lower_color  =
  match !theme with
  | "borealis"  -> {r=20.; g=16.; b=110.}
  | "redsoft"   -> {r=47.; g=72.; b=88.}
  | "tournesol" -> {r=47.; g=72.; b=88.}
  | _ -> failwith "This theme does not exist";;

let higher_color  =
  match !theme with
  | "borealis"  -> {r=0.; g=255.; b=14.}
  | "redsoft"   -> {r=217.; g=69.; b=100.}
  | "tournesol" -> {r=209.; g=217.; b=69.}
  | _ -> failwith "This theme does not exist";;

(* Open window *)
Graphics.open_graph size_string;;
Graphics.set_window_title title;;

let set_background_color =
  Graphics.set_color (Graphics.rgb (int_of_float (lower_color.r)) (int_of_float (lower_color.g)) (int_of_float (lower_color.b)));
  Graphics.fill_rect 0 0 !width !height;;

let float_to_color f_num =
  let red   = (int_of_float (lower_color.r +. (f_num *. ( higher_color.r -. lower_color.r )))) in
  let green = (int_of_float (lower_color.g +. (f_num *. ( higher_color.g -. lower_color.g )))) in
  let blue  = (int_of_float (lower_color.b +. (f_num *. ( higher_color.b -. lower_color.b )))) in
  Graphics.rgb red green blue

let draw_point x y color =
  let c = float_to_color color in (* convert value in cell to a color *)
  Graphics.set_color c;
  draw_shape x y scaled_width scaled_height
  (*Graphics.fill_ellipse (x * scaled_width) (y * scaled_height) (scaled_width/2) (scaled_height/2);;*)

let print_world () =
  Array.iteri (fun i r -> Array.iteri (fun j c -> draw_point i j c) r) world

let display_info (generation: int) (time: float) =
  Graphics.moveto (1) (1);
  Graphics.fill_rect 0 0 (9 * scaled_width) (3 * scaled_height);
  let line1 = "G: " ^ string_of_int generation ^ " ; GPS=" ^ string_of_float (100. *. (1. /. time) |> Float.round |> (fun x -> x /. 100.) ) in
  let line2 = "Map: " ^ string_of_int grid ^ "x" ^ string_of_int grid ^ " ; R=" ^ string_of_int !kernel_radius in
  let line3 = "Gauss: m=" ^ string_of_float !convo_mu ^ " sd=" ^ string_of_float !convo_sigma in

  Graphics.set_color Graphics.white;
  Graphics.draw_string line1;
  Graphics.moveto (1) (1*scaled_height);
  Graphics.draw_string line2;
  Graphics.moveto (1) (2*scaled_height);
  Graphics.draw_string line3;
  ;;

let bigbang world =
  let t1 = ref (Sys.time()) in
  let rec aux ?(display=false) generation w =
    let event = Graphics.wait_next_event [ Poll ] in
    if event.Graphics.keypressed then
      match (Graphics.read_key ()) with
      | '\027' -> Graphics.clear_graph();Graphics.close_graph()
      | 'r'    -> initialize_world (); aux ~display 0 world
      | 'i'    -> aux ~display:(not display) generation world
      | _      -> ()
    else
      print_world ();
      if display then display_info generation (Sys.time() -. !t1) else ();
      t1 := Sys.time();
      next_world world;
      aux ~display (generation+1) w
  in
  aux 0 world

let () = bigbang (world)
