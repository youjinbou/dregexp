module type ELT = 
sig

  type t

  val succ    : t -> t
  val pred    : t -> t

  val compare : t -> t -> int

end

module type S =
sig

  type t

  val compare   : t -> t -> int

  val intersect : t -> t -> t
  val merge     : t -> t -> t

end

module Make(E : ELT) : S =
struct

  type 'a pair = 'a * 'a
  type e = E.t

  (* sets as a list of sequences *)
  type t = e pair list

  (* toolbox *-----------------------*)

  (* add an element to a list - no duplicate *)
  let rec add compare l c = 
    match l with
      | []    -> [c]
      | x::xs -> match compare x c with
	  | -1 -> x::(add compare xs c)
	  |  _ -> l

  (* n² combinations of f on l1 l2 elements *)
  let mapsqr compare (f: 'a -> 'b -> 'c)  (l1 : 'a list) (l2 : 'b list) : 'c list =
    let map_one (f: 'a -> 'b -> 'c) (a : 'a) (l : 'b list) : 'c list = 
      List.map (fun x -> f a x) l 
    in
    let f1 (acc : 'c list) (x : 'a) = 
      List.fold_left (fun acc x -> add compare acc x) acc (map_one f x l2)
    in
    List.fold_left f1 [] l1

  (* element sequences *----------------*)
  module ER =
  struct

    type t = e pair

    let make a b = 
      match E.compare a b with
	  x when x < 0  -> (a,b)
	| _             -> (b,a)

    let compare = Pervasives.compare

    let sort l =
      List.sort compare l

    let intersect (a1,a2) (b1,b2) : t option =
      prerr_endline "intersecting ranges";
      if a2 < b1 || b2 < a1         (* disjoint ranges *)
      then None
      else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => b1...a2 *)
      then Some (b1,a2)
      else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => a1...b2 *)
      then Some (a1,b2)
      else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => b1...b2 *)
      then Some (b1,b2)
      else Some (a1,a2)             (* b1...a1...a2...b2 => a1...a2 *)

    let merge (a1,a2) (b1,b2) : t option =
      prerr_endline "merging ranges";
      if a2 < b1 || b2 < a1         (* disjoint ranges *)
      then None
      else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => a1...b2 *)
      then Some (a1,b2)
      else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => b1...a2 *)
      then Some (b1,a2)
      else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => a1...a2 *)
      then Some (a1,a2)
      else Some (b1,b2)             (* b1...a1...a2...b2 => b1...b2 *)

    let substract (a1,a2) (b1,b2) : t list =
      prerr_endline "substracting ranges";
      if a2 < b1 || b2 < a1         (* disjoint ranges *)
      then [ (a1,a2) ]
      else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => [ a1...b1 [ *)
      then let x = E.pred b1 in if x <= a1 then [] else [ (a1,x) ]
      else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => ] b2...a2 ] *)
      then let x = E.succ b2 in if x >= a2 then [] else [ (x, a2) ]
      else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => [ a1..b1 [ ; ] b2 .. a2 ] *)
      then
	let x = E.pred b1 and y = E.succ b2 in
	if x <= a1
	then 
	  if y >= a2 then  []
	  else [ (y,a2) ]
	else
	  if y >= a2 then  [(a1,x)]
	  else [ (a1,x) ; (y,a2) ]
      else []

(*
    (* n way intersection *)
    let nintersect l : t option =
      let rec nintersect l r =
	match l with
	  | []    -> Some r
	  | x::xs -> (
	    match intersect x r with
	      | None   -> None
	      | Some r -> nintersect xs r
	  )
      in match l with
	| x::xs -> nintersect xs x
	| []    -> None


    (* n way merge *)
    let nmerge l =
      let rec nmerge l i r = 
	match l with
	  | []    -> i::r
	  | x::xs -> (
	    match merge x i with
		None    -> nmerge xs x (i::r)
	      | Some cr -> nmerge xs cr r
	  )
      in match sort l with
	  []    -> []
	| x::xs -> nmerge xs x []
*)

  end

  (* signature symbols *----------------------------------------*)

  let compare = Pervasives.compare

  let intersect (l1 : t) (l2 : t) : t = 
    let rec intersect l1 l2 r =
      match l1, l2 with
	|  _, []
	| [], _                    -> List.rev r
	| (a1,a2)::xs, (b1,b2)::ys -> (
	  if a2 < b1                    (* a1...a2...b1...b2 => [] *)
	  then intersect xs l2 r
	  else if b2 < a1               (* b1...b2...a1...a2 => [] *)
	  then intersect l1 ys r
	  else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => b1...a2 *)
	  then intersect xs l2 ((b1,a2)::r)
	  else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => a1...b2 *)
	  then intersect l1 ys ((a1,b2)::r)
	  else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => b1...b2 *)
	  then intersect l1 ys ((b1,b2)::r)
          else                          (* b1...a1...a2...b2 => a1...a2 *)
	    intersect xs l2 ((a1,a2)::r)
	)
    in intersect l1 l2 []

  let merge (l1 : t) (l2 : t) : t = 
    let rec merge l1 l2 r =
      match l1, l2 with
	|  lr, []
	| [], lr                   -> List.append (List.rev r) lr
	| (a1,a2)::xs, (b1,b2)::ys -> (
	  if a2 < b1                    (* a1...a2...b1...b2 => [] *)
	  then merge xs l2 ((a1,a2)::r)
	  else if b2 < a1               (* b1...b2...a1...a2 => [] *)
	  then merge l1 ys ((b1,b2)::r)
	  else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => a1...b2 *)
	  then merge ((a1,b2)::xs) l2 r
	  else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => b1...a2 *)
	  then merge ((b1,a2)::xs) ys r
	  else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => a1...a2 *)
	  then merge ((a1,a2)::xs) ys r
	  else                          (* b1...a1...a2...b2 => b1...b2 *)
	    merge ((b1,b2)::xs) ys r
	)
    in merge l1 l2 []

  let substract (l1 : t) (l2 : t) : t =
    let rec substract l1 l2 r = 
      match l1, l2 with
	|  lr, []                  -> List.append (List.rev r) lr
	| [], lr                   -> List.rev r
	| (a1,a2)::xs, (b1,b2)::ys -> (
      if a2 < b1                    (* a1...a2...b1...b2 => [a1..a2] *)
      then substract xs l2 ((a1,a2)::r)
      else if b2 < a1               (* b1...b2...a1...a2 => [a1..a2] *)
      then substract xs ys ((a1,a2)::r)
      else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => [ a1...b1 [ *)
      then 
	let x = E.pred b1 in
	if x <= a1
	then substract xs l2 r
	else substract xs l2 ((a1,x)::r)
      else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => ] b2...a2 ] *)
      then 
	let x = E.succ b2 in
	if x >= a2
	then substract xs ys r
	else substract ((x, a2)::xs) ys r
      else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => [ a1..b1 [ ; ] b2 .. a2 ] *)
      then
	let x = E.pred b1 and y = E.succ b2 in
	if x <= a1
	then 
	  if y >= a2 
	  then substract xs ys r
	  else substract xs ys ((y,a2)::r)
	else
	  if y >= a2 
	  then substract xs ys ((a1,x)::r)
	  else substract xs ys ((y,a2)::(a1,x)::r)
      else substract xs l2 r        (* b1...a1...a2...b2 => [] *)
	)
    in substract l1 l2 []

end
