(* dune exec ./snake.exe *)

(* A simple snake game in the terminal using OCaml.
   Controls: Arrow keys or ZQSD (Z=Up, Q=Left, S=Down, D=Right)
   Quit: L key *)

(* escape sequences : 
  "\027[31mA\027[0m"
  \027 : escape
  [31m : set color to red
  A : the character to display
  \027[0m : reset color
*)

open Unix

let esc = "\027["

(* ansi escape colors *)
let _black = esc ^ "30m"
let red = esc ^ "31m"
let green = esc ^ "32m"
let _yellow = esc ^ "33m"
let _blue = esc ^ "34m"
let _magenta = esc ^ "35m"
let _cyan = esc ^ "36m"
let _white = esc ^ "37m"
let reset = esc ^ "0m"

(* ---- terminal raw mode ---- *)
let orig_attr = ref None

let show_cursor = print_string (esc ^ "?25h")
let hide_cursor = print_string (esc ^ "?25l")
let clear_screen () = print_string (esc ^ "2J" ^ esc ^ "H")

let enable_raw () =
  let fd = descr_of_in_channel Stdlib.stdin in
  let a = tcgetattr fd in
  orig_attr := Some a;
  let b = { a with c_icanon = false; c_echo = false; c_vmin = 0; c_vtime = 0 } in
  tcsetattr fd TCSANOW b

let restore_terminal () = (* restore terminal settings *)
  (match !orig_attr with
   | None -> ()
   | Some a ->
       let fd = descr_of_in_channel Stdlib.stdin in
       tcsetattr fd TCSANOW a);
  show_cursor;
  flush Stdlib.stdout

let () = at_exit restore_terminal

let draw_box w h body apple = (* draw box in unix terminal *)
  clear_screen ();
  let grid = Array.init h (fun _ -> Array.make w " ") in

  (* apple *)
  let (ax, ay) = apple in
  if ay >= 0 && ay < h && ax >= 0 && ax < w then grid.(ay).(ax) <- (red ^ "@" ^ reset); (* apple: @ in red *)

  (* snake *)
  let segs = Deque.to_list body in
    (match segs with
    | [] -> ()
    | hd :: tl ->
       let (hx, hy) = hd in
       if hy >= 0 && hy < h && hx >= 0 && hx < w then grid.(hy).(hx) <- (green ^ "X" ^ reset); (* head: X in green *)
       List.iter (fun (x, y) ->
           if y >= 0 && y < h && x >= 0 && x < w then grid.(y).(x) <- (green ^ "o" ^ reset); (* body: o in green *)
         ) tl);

  (* top border *)
  Printf.printf "+";
  for _ = 0 to w - 1 do Printf.printf "-" done;
  Printf.printf "+\n";

  (* content *)
  for y = 0 to h - 1 do
    Printf.printf "|";
    for x = 0 to w - 1 do Printf.printf "%s" grid.(y).(x) done;
    Printf.printf "|\n";
  done;

  (* bottom border *)
  Printf.printf "+";
  for _ = 0 to w - 1 do Printf.printf "-" done;
  Printf.printf "+\n";

  flush Stdlib.stdout

type dir = Up | Down | Left | Right | NoDir (* new direction type *)

let read_available () : string option = (* read available input *)
  let fd = descr_of_in_channel Stdlib.stdin in
  let ready,_,_ = select [fd] [] [] 0.0 in
  if ready = [] then None
  else
    let buf = Bytes.create 16 in
    let n = read fd buf 0 16 in
    if n <= 0 then None else Some (Bytes.sub_string buf 0 n)

let parse_input (s:string) : dir * bool = (* parse user input *)
  (* returns (direction, quit) *)
  let quit = String.exists (fun c -> c='l' || c='L') s in
  (* Arrow keys: ESC [ A/B/C/D *)
  let d =
    if String.length s >= 3 && s.[0]='\027' && s.[1]='[' then
      match s.[2] with
      | 'A' -> Up | 'B' -> Down | 'C' -> Right | 'D' -> Left | _ -> NoDir
    else
      (* ZQSD *)
      match s with
      | _ when String.exists (fun c -> c='z' || c='Z') s -> Up
      | _ when String.exists (fun c -> c='s' || c='S') s -> Down
      | _ when String.exists (fun c -> c='q' || c='Q') s -> Left
      | _ when String.exists (fun c -> c='d' || c='D') s -> Right
      | _ -> NoDir
  in
  (d, quit)

let spawn_apple body w h = (* spawn 1 new apple position *)
  let taken = Deque.to_list body in
  let rec pick () =
    let x = Random.int w in
    let y = Random.int h in
    if List.mem (x, y) taken then pick () else (x, y) (* pick a new position if taken (call pick recursively) *)
  in
  pick ()

(* let string_of_dir d =
  match d with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | NoDir -> "NoDir" *)

(* ---- main loop ---- *)
let () =
  enable_raw ();
  Random.self_init ();

  let w, h = 25, 25 in (* dimensions of the unix box *)
  let dir_xy  = ref Up in
  let x = ref (w / 2) in
  let y = ref (h / 2) in
  let body = Deque.create () in
  for _ = 1 to 3 do
    Deque.push_front body (!x, !y)
  done;
  let apple = ref (spawn_apple body w h) in

  let clamp v lo hi = max lo (min hi v) in
  
  let apply_dir = function
    | Up    -> y := clamp (!y - 1) 0 (h - 1)
    | Down  -> y := clamp (!y + 1) 0 (h - 1)
    | Left  -> x := clamp (!x - 1) 0 (w - 1)
    | Right -> x := clamp (!x + 1) 0 (w - 1)
    | NoDir -> ()
  in
  
  let period = 0.2 in (* seconds *)
  let running = ref true in
  while !running do
    let t0 = Unix.gettimeofday () in
    (* check for user input *)
    let fd = descr_of_in_channel Stdlib.stdin in
    let readable, _, _ = Unix.select [fd] [] [] period in

    (* if there's input, drain all available inputs (non-blocking) *)
    if readable <> [] then begin
      let rec drain_inputs () =
        match read_available () with
        | None -> ()          (* no more available right now *)
        | Some s ->
           let d, quit = parse_input s in
            if d <> NoDir && d <> !dir_xy then dir_xy := d;
            if quit then running := false;
            drain_inputs ()
      in
      drain_inputs ()
    end;

    (* do the work once per period *)
    apply_dir !dir_xy; (* apply direction *)
    let new_head = (!x, !y) in (* new head position *)
    match new_head with
    | hx, hy when hx < 0 || hx >= w || hy < 0 || hy >= h ->
        running := false  (* hit wall *)
    | _ when List.mem new_head (Deque.to_list body) ->
        running := false  (* hit itself *)
    | _ -> begin
      let new_head = (!x, !y) in (* new head position *)
      Deque.push_front body new_head; (* add new head to the snake body *)
      if new_head = !apple then ( (* if the snake eats the apple *)
        apple := spawn_apple body w h (* spawn a new apple *)
      ) else ignore (Deque.pop_back body); (* remove last segment of the snake body *)

      draw_box w h body !apple; (* draw the box *)
      Printf.printf "len=%d\n%!" (Deque.length body);
      hide_cursor;

      (* keep stable rate: sleep remaining time if any *)
      let elapsed = Unix.gettimeofday () -. t0 in
      if elapsed < period then Unix.sleepf (period -. elapsed) else () end; 

  done
