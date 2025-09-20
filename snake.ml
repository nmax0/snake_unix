(*
dune exec ./snake.exe
 *)
(* \027 = ESC *)

open Unix

(* ---- terminal raw mode ---- *)
let orig_attr = ref None

let enable_raw () =
  let fd = descr_of_in_channel Stdlib.stdin in
  let a = tcgetattr fd in
  orig_attr := Some a;
  let b = { a with c_icanon = false; c_echo = false; c_vmin = 0; c_vtime = 0 } in
  tcsetattr fd TCSANOW b

let restore_terminal () =
  (match !orig_attr with
   | None -> ()
   | Some a ->
       let fd = descr_of_in_channel Stdlib.stdin in
       tcsetattr fd TCSANOW a);
  print_string "\027[?25h";
  flush Stdlib.stdout

let () = at_exit restore_terminal

let clear_screen () = Printf.printf "\027[2J\027[H"

let draw_box w h body apple =
  clear_screen ();
  let grid = Array.init h (fun _ -> Array.make w " ") in

  (* apple *)
  let (ax, ay) = apple in
  if ay >= 0 && ay < h && ax >= 0 && ax < w then grid.(ay).(ax) <- "@";

  (* snake *)
  let segs = Deque.to_list body in
    (match segs with
    | [] -> ()
    | hd :: tl ->
       let (hx, hy) = hd in
       if hy >= 0 && hy < h && hx >= 0 && hx < w then grid.(hy).(hx) <- "X";
       List.iter (fun (x, y) ->
           if y >= 0 && y < h && x >= 0 && x < w then grid.(y).(x) <- "o"
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

type dir = Up | Down | Left | Right | NoDir

let read_available () : string option =
  let fd = descr_of_in_channel Stdlib.stdin in
  let ready,_,_ = select [fd] [] [] 0.0 in
  if ready = [] then None
  else
    let buf = Bytes.create 16 in
    let n = read fd buf 0 16 in
    if n <= 0 then None else Some (Bytes.sub_string buf 0 n)

let parse_input (s:string) : dir * bool =
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

let spawn_apple body w h =
  let taken = Deque.to_list body in
  let rec pick () =
    let x = Random.int w in
    let y = Random.int h in
    if List.mem (x, y) taken then pick () else (x, y)
  in
  pick ()

let string_of_dir d =
  match d with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | NoDir -> "NoDir"

(* ---- main loop ---- *)
let () =
  enable_raw ();
  Random.self_init ();

  let w, h = 25, 25 in
  let dir_xy  = ref Up in
  let x = ref (w / 2) in
  let y = ref (h / 2) in
  let body = Deque.create () in
  Deque.push_front body (!x, !y);
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
    apply_dir !dir_xy;
    let new_head = (!x, !y) in
    Deque.push_front body new_head;
    if new_head = !apple then (
      apple := spawn_apple body w h
    ) else ignore (Deque.pop_back body);
    
    draw_box w h body !apple;
    Printf.printf "%s\n%!" (string_of_dir !dir_xy);
    Printf.printf "len=%d\n%!" (Deque.length body);
    Deque.iter body (fun (x, y) -> Printf.printf "(%d,%d)\n%!" x y);

    (* keep stable rate: sleep remaining time if any *)
    let elapsed = Unix.gettimeofday () -. t0 in
    if elapsed < period then Unix.sleepf (period -. elapsed) else ()

  done
