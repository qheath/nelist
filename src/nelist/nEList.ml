type 'a t = 'a * 'a list


(* Manipulation/transformation *)

let rev_append =
  let rec aux (h0,t0) = function
    | [] -> h0,t0
    | h1::t1 -> aux (h1,h0::t0) t1
  in
  fun l (h,t) -> aux (h,t) l

let fold f g =
  let rec aux accum = function
    | [] -> accum
    | h1::t1 -> aux (f h1 (g h1 accum)) t1
  in
  fun (h,t) seed -> aux (f h seed) t

let rev_map f nel =
  fold
    (fun h accum -> f h,accum)
    (fun _ (h,t) -> h::t)
    nel []

let rev (h,t) =
  rev_append t (h,[])

let map f nel =
  rev_map f @@ rev nel

let flatten =
  let rec aux (hh0,ht0) = function
    | (hh1,ht1),[] -> rev_append (hh0::ht0) (hh1,ht1)
    | (hh1,ht1),(h::t) -> aux (rev_append (hh1::ht1) (hh0,ht0)) (h,t)
  in
  function
  | h,[] -> h
  | h0,(h1::t1) -> aux (rev h0) (h1,t1)


(* Creation *)

let init n f =
  if n<1 then raise (Invalid_argument "NEList.init") else
    let h = f 0 in
    let t = List.init (n-1) (fun i -> f (i+1)) in
    h,t

let push h0 = function
  | Some (h1,t) -> h0,h1::t
  | None -> h0,[]

let push_back nel' h0 =
  let len' = match nel' with
    | None -> None
    | Some nel -> Some (rev nel)
  in
  rev @@ push h0 len'

let of_list = function
  | [] -> None
  | h::t -> Some (h,t)


(* Destruction *)

let ends (h,t) =
  let h',_ = rev (h,t) in
  h,h'

let iter f g nel =
  let _ =
    fold
      (fun h () -> f h ; h)
      (fun h1 h0 -> g h0 h1)
      nel ()
  in
  ()

let pop = function
  | h,[] -> h,None
  | h1,h2::t -> h1,Some (h2,t)

let pop_back nel =
  let h,len' = pop @@ rev nel in
  let nel' = match len' with
  | None -> None
  | Some nel -> Some (rev nel)
  in
  nel',h

let to_list (h,t) = h::t

let length (_,l) =
  1 + List.length l
