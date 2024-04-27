open Probzelus
open Ztypes
open Effect
open Effect.Deep

(** Nested inference : possible improvement
  - Do a GADT to deal with program of different kind
  - Do a GADT to better treat the arguments of the nested infered functions *)

type in_a = Engine.board * Engine.player
type out_a = Engine.move
type in_b = Engine.board * Engine.player * Engine.move
type out_b = float
type _ Effect.t += Nested_infer_alice : in_a -> out_a Distribution.t t
type _ Effect.t += Nested_infer_bob : in_b -> out_b Distribution.t t

let nested_infer_alice (args : in_a) : out_a Distribution.t =
  perform (Nested_infer_alice args)

let nested_infer_bob (args : in_b) : out_b Distribution.t =
  perform (Nested_infer_bob args)

let do_infer_alice (alice : (Infer_pf.prob * in_a, out_a) cnode)
    (bob : (Infer_pf.prob * in_b, out_b) cnode) :
    (in_a, out_a Distribution.t) cnode =
  let (Cnode { alloc = alloc_a; copy = copy_a; step = step_a; reset = reset_a })
      =
    Infer_pf.infer 100 alice
  in
  let (Cnode { alloc = alloc_b; copy = copy_b; step = step_b; reset = reset_b })
      =
    Infer_pf.infer 100 bob
  in
  let alloc () = (alloc_a (), alloc_b ()) in
  let copy (s_a, s_b) (s_a', s_b') =
    copy_a s_a s_a';
    copy_b s_b s_b'
  in
  let rec step_alice (s_a, s_b) (arg : in_a) : out_a Distribution.t =
    try_with (step_a s_a) arg
      {
        effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Nested_infer_alice arg ->
                Some
                  (fun (k : (a, _) continuation) ->
                    print_endline "Alice, Alice";
                    continue k (step_alice (alloc_a (), s_b) arg))
            | Nested_infer_bob arg ->
                Some
                  (fun (k : (a, _) continuation) ->
                    print_endline "Alice, Bob";
                    continue k (step_bob (s_a, alloc_b ()) arg))
            | _ -> None);
      }
  and step_bob (s_a, s_b) (arg : in_b) : out_b Distribution.t =
    try_with
      (step_b (alloc_b ()))
      arg
      {
        effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Nested_infer_alice arg ->
                Some
                  (fun (k : (a, _) continuation) ->
                    print_endline "Bob, Alice";
                    continue k (step_alice (alloc_a (), s_b) arg))
            | Nested_infer_bob arg ->
                Some
                  (fun (k : (a, _) continuation) ->
                    print_endline "Bob, Bob";
                    continue k (step_bob (s_a, alloc_b ()) arg))
            | _ -> None);
      }
  in
  let reset (s_a, s_b) =
    reset_a s_a;
    reset_b s_b
  in
  Cnode { alloc; copy; step = step_alice; reset }

(* type 'a difflist = *)
(*   | ( :: ) : 'hd * 'tl difflist -> ('hd * 'tl) difflist *)
(*   | [] : unit difflist *)

(* let do_nested_infer (type a b) (programs : (Infer_pf.prob * a, b) cnode List.t) *)
(*     : (a, b Distribution.t) cnode = *)
(*   let run_program (i : int) : (a, b Distribution.t) cnode = *)
(*     let (Cnode { alloc; copy; step; reset }) = List.nth programs i in *)
(*     let alloc () = *)
(*       let rec aux (type s) (l : (Infer_pf.prob * a, b) cnode List.t) : s difflist = *)
(*         (match l with | [] -> [] | _::_ -> _) in *)
(*     let rec step_nested s (arg : a) : b Distribution.t = *)
(*       let step sa = step (fst sa) (snd sa) in *)
(*       try_with step (s, arg) *)
(*         { *)
(*           effc = *)
(*             (fun (type a) (eff : a t) -> *)
(*               match eff with *)
(*               | Nested_infer (i, a) -> *)
(*                   Some *)
(*                     (fun (k : (a, b Distribution.t) continuation) -> *)
(*                       continue k _) *)
(*               | _ -> None); *)
(*         } *)
(*     in *)
(*     Cnode { alloc; copy; step = step_nested; reset } *)
(*   in *)
(*   run_program 0 *)
