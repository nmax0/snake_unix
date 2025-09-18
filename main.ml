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
  for _=1 to w do print_string "-" done;
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
  for _=1 to w do print_string "-" done;
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

(* ---- main loop ---- *)
let () =
  enable_raw ();
  Random.self_init ();

  let w, h = 30, 15 in
  let x = ref (w/2) and y = ref (h/2) in
  let running = ref true in

  let clamp v lo hi = max lo (min hi v) in
  let apply_dir = function
    | Up    -> y := clamp (!y - 1) 0 (h-1)
    | Down  -> y := clamp (!y + 1) 0 (h-1)
    | Left  -> x := clamp (!x - 1) 0 (w-1)
    | Right -> x := clamp (!x + 1) 0 (w-1)
    | None  -> ()
  in

  let target_fps = 20.0 in
  let dt = 1.0 /. target_fps in

  while !running do
    (* input *)
    (match read_available () with
     | None -> ()
     | Some s ->
         let d, quit = parse_input s in
         if quit then running := false;
         apply_dir d);

    (* render *)
    draw_box w h (!x, !y);

    (* throttle CPU *)
    sleepf dt
  done;

  print_endline "\nBye!";
