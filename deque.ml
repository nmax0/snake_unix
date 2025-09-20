(*
  deque.ml
  Simple two-list deque (mutable container).
  Invariant / convention used here:
    - `front` stores elements from head -> middle (head is List.hd front when non-empty)
    - `back` stores elements from middle -> tail but *reversed* (head of `back` is the logical tail)
  The full logical sequence (head..tail) = front @ (List.rev back)
 *)

type 'a t = {
    mutable front : 'a list;
    mutable back  : 'a list;
  }

let create () = { front = []; back = [] }

let to_list d = d.front @ (List.rev d.back)

let length d = List.length d.front + List.length d.back

let is_empty d = match d.front, d.back with [], [] -> true | _ -> false

let iter d fn =
  List.iter fn d.front;
  List.iter fn (List.rev d.back)

let push_front d x =
  d.front <- x :: d.front (* so d.front is now x + d.front *)

let push_back d x =
  d.back <- x :: d.back

let pop_front d =
  match d.front with
  | hd::tl -> d.front <- tl; Some hd (* hd is the head and tail is the rest *)
  | [] ->
     begin match List.rev d.back with
     | [] -> None
     | hd::tl ->
        d.front <- tl;
        d.back <- [];
        Some hd
     end


let pop_back d =
  match d.back with
  | hd::tl -> d.back <- tl; Some hd (* hd is the head and tail is the rest *)
  | [] ->
     begin match List.rev d.front with
     | [] -> None
     | hd::tl ->
        d.back <- tl;
        d.front <- [];
        Some hd
     end

let get_head d =
  match d.front with
  | hd::_ -> Some hd
  | [] ->
     let rec last_elem = function
       | [] -> None
       | [x] -> Some x
       | _ :: tl -> last_elem tl in
     last_elem d.back
