(** This file provides an implementation to print the tic tac toe game *)

open Graphics
open Probzelus

type pos = int * int

(** [draw_cross factor x y] draws a cross at position [(x,y)] of size [2*factor] *)
let draw_cross ?(color = blue) (factor : int) (x : int) (y : int) =
  set_color color;
  let recenter () = moveto x y in
  List.iter
    (fun (u, v) ->
      recenter ();
      rlineto u v)
    (List.map
       (fun (u, v) -> (factor * u, factor * v))
       [ (1, 1); (1, -1); (-1, -1); (-1, 1) ])

(** [draw_circle factor x y] draws a circle with radius [factor] and center [(x,y)] *)
let draw_circle ?(color = red) (factor : int) (x : int) (y : int) =
  set_color color;
  draw_circle x y factor

(** [draw_player factor n p (x , y)] draws either a circle or a cross (depending of [p]) at position [(x,y)] *)
let draw_player (factor : int) (n : int) (p : Engine.player)
    ((i, j) : Engine.move) : unit =
  let x = ((2 * j) + 1) * factor in
  let y = ((2 * (n - i)) - 1) * factor in
  moveto x y;
  match p with
  | Engine.Cross -> draw_cross factor x y
  | Engine.Circle -> draw_circle factor x y

(** [draw_player_proba factor p (x , y) proba] draws either a circle or a cross (depending of [p]) at position [(x,y)] with color intensity proportional to proba*)
let draw_player_proba (factor : int) (n : int) (p : Engine.player)
    ((i, j) : Engine.move) (proba : float) : unit =
  let x = ((2 * j) + 1) * factor in
  let y = ((2 * (n - i)) - 1) * factor in
  let r = int_of_float @@ ((1. -. proba) *. 255.) in
  let color = rgb r r r in
  moveto x y;
  set_color black;
  draw_string @@ Format.sprintf "%.3f" proba;
  moveto x y;
  match p with
  | Engine.Cross -> draw_cross ~color factor x y
  | Engine.Circle -> draw_circle ~color factor x y

(** [draw_grid factor n] draws the grid of size [n] *)
let draw_grid (factor : int) (n : int) : unit =
  set_color black;
  let moveto x y : unit = moveto (2 * factor * x) (2 * factor * y) in
  let lineto x y : unit = lineto (2 * factor * x) (2 * factor * y) in
  for i = 0 to n do
    moveto 0 i;
    lineto n i;
    moveto i 0;
    lineto i n
  done

(** [draw_board factor b] draws the board [s] of the game*)
let draw_board (factor : int) (s : Engine.board) : unit =
  let n = Array.length s in
  assert (n > 0);
  let m = Array.length s.(0) in
  assert (n = m);
  clear_graph ();
  draw_grid factor n;

  Array.iteri
    (fun i l ->
      Array.iteri
        (fun j m ->
          match m with Some p -> draw_player factor n p (i, j) | None -> ())
        l)
    s

(** [draw_move_dist factor b p dist] draws the board [s] and the distribution dist *)
let draw_move_dist (factor : int) (s : Engine.board) (p : Engine.player)
    (dist : Engine.move Distribution.t) =
  let n = Array.length s in
  draw_board factor s;
  let rec _draw_move (m_l : Engine.move list) =
    match m_l with
    | [] -> ()
    | m :: l ->
        let proba = exp (Distribution.score (dist, m)) in
        draw_player_proba factor n p m proba;
        _draw_move l
  in
  _draw_move (Engine.valid_moves s)

let pos_to_move (factor : int) (n : int) (x : int) (y : int) : Engine.move =
  (n - ((y / (2 * factor)) + 1), x / (2 * factor))
