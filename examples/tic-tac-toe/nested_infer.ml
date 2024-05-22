open Probzelus
open Ztypes
open Infer_pf

let print_player (p : Engine.player) =
  match p with
  | Engine.Cross -> print_endline "Cross"
  | Engine.Circle -> print_endline "Circle"

let draw = Distribution.draw
let mean_float = Distribution.mean_float

let map (f : 'a * 'b -> 'c) (b : 'b) : 'a Distribution.t -> 'c Distribution.t =
  Distribution.map (fun a -> f (a, b))

let apply : ('a -> 'b) * 'a -> 'b = fun (f, a) -> f a

let _infer n (Cnode { alloc; reset; copy; step }) =
  let alloc () =
    {
      infer_states = Array.init n (fun _ -> alloc ());
      infer_scores = Array.make n 0.0;
    }
  in
  let reset state =
    Array.iter reset state.infer_states;
    Array.fill state.infer_scores 0 n 0.0
  in
  let step { infer_states = states; infer_scores = scores } input =
    let values =
      Array.mapi
        (fun i state ->
          let value = step state ({ idx = i; scores }, input) in
          value)
        states
    in
    let _, ret = Normalize.normalize_nohist values scores in
    Array.fill scores 0 n 0.0;
    ret
  in
  let copy src dst =
    for i = 0 to n - 1 do
      copy src.infer_states.(i) dst.infer_states.(i);
      dst.infer_scores.(i) <- src.infer_scores.(i)
    done
  in
  Cnode { alloc; reset; copy; step }

let nested_infer (depth : int) (n_part : int)
    (alice :
      ('in_b, 'out_b Distribution.t) cnode -> (prob * 'in_a, 'out_a) cnode)
    (bob : ('in_a, 'out_a Distribution.t) cnode -> (prob * 'in_b, 'out_b) cnode)
    (dummy_alice : 'in_a -> 'out_a Distribution.t)
    (dummy_bob : 'in_b -> 'out_b Distribution.t) :
    ('in_a, 'out_a Distribution.t) cnode =
  let (Cnode { alloc = alloc_a; copy = copy_a; step = step_a; reset = reset_a })
      =
    infer n_part alice
  in
  let (Cnode { alloc = alloc_b; copy = copy_b; step = step_b; reset = reset_b })
      =
    infer n_part bob
  in
  let alloc () = (alloc_a (), alloc_b ()) in
  let copy (s_a, s_b) (s_a', s_b') =
    copy_a s_a s_a';
    copy_b s_b s_b'
  in
  let step =
    let _memoize_step_a table (s_a, s_b) (f, x) =
      (* try Hashtbl.find table x *)
      (* with Not_found -> *)
      let o = step_a s_a (f, x) in
      (* Hashtbl.add table x o; *)
      o
    in
    let _memoize_step_b table (s_a, s_b) (f, x) =
      (* try Hashtbl.find table x *)
      (* with Not_found -> *)
      let o = step_b s_b (f, x) in
      (* Hashtbl.add table x o; *)
      o
    in
    let rec _step_a (depth : int) (s_a, s_b) =
      (* let table = Hashtbl.create 100 in *)
      match depth with
      | 0 -> dummy_alice
      | n ->
          print_endline (string_of_int n);
          fun in_a -> step_a s_a (_step_b (depth - 1) (s_a, s_b), in_a)
    and _step_b (depth : int) (s_a, s_b) =
      (* let table = Hashtbl.create 100 in *)
      match depth with
      | 0 -> dummy_bob
      | n ->
          print_endline (string_of_int n);
          fun in_b -> step_b s_b (_step_a depth (s_a, s_b), in_b)
    in
    _step_a depth
  in
  let reset (s_a, s_b) =
    reset_a s_a;
    reset_b s_b
  in
  Cnode { alloc; copy; step; reset }
