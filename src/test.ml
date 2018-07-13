let print_short nel =
  match NEList.pop nel with
  | _,None -> ()
  | h0,Some nel0 ->
    let h1,hn = NEList.ends nel0 in
    Printf.printf " = [ %d ; %d ; ... ; %d ]" h0 h1 hn

let print name nel =
  Printf.printf "%s(%d)" name (NEList.length nel) ;
  print_short nel ;
  Printf.printf " = [" ;
  NEList.iter
    (Printf.printf " %d")
    (fun _ _ -> Printf.printf " ;")
    nel ;
  Printf.printf " ]" ;
  Printf.printf "\n"

let () =
  let l0 = NEList.push 0 None in
  print "l0" l0 ;
  let l1 = NEList.push 1 (Some l0) in
  print "l1" l1 ;
  let l2 = NEList.init 5 (fun i -> i+5) in
  print "l2" l2 ;
  let l3 =
    match NEList.of_list [2;3;4] with
    | Some l3 -> l3
    | None -> assert false
  in
  print "l3" l3 ;

  let l4 = NEList.flatten @@ NEList.push l1 (NEList.of_list [l2;l3]) in
  print "flattened" l4 ;

  let l5 = NEList.rev_map (fun i -> i+10) l4 in
  print "rev-mapped" l5 ;

  let l6 = NEList.rev_append (NEList.to_list l5) l4 in
  print "rev-appended" l6 ;
  Printf.printf "%!"
