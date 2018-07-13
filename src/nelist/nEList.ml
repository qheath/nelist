type 'a t = 'a * 'a list

(* Creation *)

let init n f =
  if n<1 then raise (Invalid_argument "NEList.init") else
    let h = f 0 in
    let t = List.init (n-1) (fun i -> f (i+1)) in
    h,t

let push h0 = function
  | Some (h1,t) -> h0,h1::t
  | None -> h0,[]

let of_list = function
  | [] -> None
  | h::t -> Some (h,t)

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
    | (hh1::ht1),t -> aux (hh1,hh0::ht0) (ht1,t)
    | [],((hh1,ht1)::t) -> aux (hh1,hh0::ht0) (ht1,t)
    | [],[] -> rev (hh0,ht0)
  in
  fun ((hh,ht),t) -> aux (hh,[]) (ht,t)

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

let to_list (h,t) = h::t

let length (_,l) =
  1 + List.length l
