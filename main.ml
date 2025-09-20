(*
  ocamlc -o main -I +unix unix.cma main.ml
./main
 *)

(* draw.ml — first rendering with Unix *)

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
  (* show cursor back *)
  print_string "\027[?25h"; flush Stdlib.stdout

let () = at_exit restore_terminal

let clear_screen () =
  (* \027 = ESC *)
  print_string "\027[2J";   (* clear *)
  print_string "\027[H";    (* move cursor to home (0,0) *)
  flush Stdlib.stdout

let draw_box w h (px, py) =
  clear_screen ();
  (* top border *)
  print_string "+";
  for _=1 to w do print_string "—" done;
  print_string "+\n";

  (* inside *)
  for y=0 to h-1 do
    print_string "|";
    for x=0 to w-1 do
      if (x,y) = (px,py) then
        print_string "@"
      else
        print_string " "
    done;
    print_string "|\n";
  done;
  
  (* bottom border *)
 print_string "+";
  for _=1 to w do print_string "—" done;
  print_string "+\n";
  flush Stdlib.stdout

(* ---- input ---- *)
type dir = Up | Down | Left | Right | None

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
  let quit = String.exists (fun c -> c='a' || c='A') s in
  (* Arrow keys: ESC [ A/B/C/D *)
  let d =
    if String.length s >= 3 && s.[0]='\027' && s.[1]='[' then
      match s.[2] with
      | 'A' -> Up | 'B' -> Down | 'C' -> Right | 'D' -> Left | _ -> None
    else
      (* ZQSD *)
      match s with
      | _ when String.exists (fun c -> c='z' || c='Z') s -> Up
      | _ when String.exists (fun c -> c='s' || c='S') s -> Down
      | _ when String.exists (fun c -> c='q' || c='Q') s -> Left
      | _ when String.exists (fun c -> c='d' || c='D') s -> Right
      | _ -> None
  in
  (d, quit)


let string_of_dir d =
  match d with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | None -> "None"

(* ---- main loop ---- *)
let () =
  enable_raw ();
  Random.self_init ();

  let w, h = 30, 20 in
  let x = ref (w/2) and y = ref (h/2) in
  let dir_xy  = ref Up in
  let running = ref true in

  let clamp v lo hi = max lo (min hi v) in
  let apply_dir = function
    | Up    -> y := clamp (! y - 1) 0 (h-1)
    | Down  -> y := clamp (!y + 1) 0 (h-1)
    | Left  -> x := clamp (!x - 1) 0 (w-1)
    | Right -> x := clamp (!x + 1) 0 (w-1)
    | None  -> ()
  in

  let period = 1.0 in (* seconds *)
  
  while !running do
    let t0 = Unix.gettimeofday () in
    let readable, _, _ = Unix.select [Unix.stdin] [] [] period in

    (* if there's input, drain all available inputs (non-blocking) *)
    if readable <> [] then begin
      let rec drain_inputs () =
        
        match read_available () with
        | None -> ()          (* no more available right now *)
        | Some s ->
           let d, quit = parse_input s in
            if d <> None && d <> !dir_xy then dir_xy := d;
            if quit then running := false;
            drain_inputs ()
      in
      drain_inputs () end;

    (* do the periodic work once per period *)
    apply_dir !dir_xy;
    draw_box w h (!x, !y);
    Printf.printf "%d, %d\n%!" !x !y;

    (* keep stable rate: sleep remaining time if any *)
    let elapsed = Unix.gettimeofday () -. t0 in
    if elapsed < period then Unix.sleepf (period -. elapsed) else ()

  done;

  print_endline "\nBye!";
