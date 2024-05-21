open Probzelus
open Ztypes

let mutual_infer (depth : int) (n_part : int)
    (alice :
      (Infer_pf.prob * (('in_b -> 'out_b Distribution.t) * 'in_a), 'out_a) cnode)
    (bob :
      (Infer_pf.prob * (('in_a -> 'out_a Distribution.t) * 'in_b), 'out_b) cnode)
    (dummy_alice : 'in_a -> 'out_a Distribution.t)
    (dummy_bob : 'in_b -> 'out_b Distribution.t) :
    ('in_a, 'out_a Distribution.t) cnode =
  let (Cnode { alloc = alloc_a; copy = copy_a; step = step_a; reset = reset_a })
      =
    Infer_pf.infer n_part alice
  in
  let (Cnode { alloc = alloc_b; copy = copy_b; step = step_b; reset = reset_b })
      =
    Infer_pf.infer n_part bob
  in
  let alloc () = (alloc_a (), alloc_b ()) in
  let copy (s_a, s_b) (s_a', s_b') =
    copy_a s_a s_a';
    copy_b s_b s_b'
  in
  let step =
    let rec _step_a (depth : int) (s_a, s_b) =
      match depth with
      | 0 -> dummy_alice
      | n -> fun in_a -> step_a s_a (_step_b (depth - 1) (s_a, s_b), in_a)
    and _step_b (depth : int) (s_a, s_b) =
      match depth with
      | 0 -> dummy_bob
      | n -> fun in_b -> step_b s_b (_step_a depth (s_a, s_b), in_b)
    in
    _step_a depth
  in
  let reset (s_a, s_b) =
    reset_a s_a;
    reset_b s_b
  in
  Cnode { alloc; copy; step; reset }
