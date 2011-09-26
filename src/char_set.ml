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
  val merge     : t -> t -> t list

end



module Make(E : ELT) : S =
struct

  type 'a pair = 'a * 'a
  type e = E.t

  type t = 
      Set   of e list
    | Range of e pair


  (* element ranges *)
  module ER =
  struct

    type t = e pair

    let make a b = 
      match E.compare a b with
	  x when x < 0  -> Range (a,b)
	| x when x > 0  -> Range (b,a)
	| _             -> Set [a]

    let compare (x,x') (y,y') = 
      let c = E.compare x y in
      if c = 0 then E.compare x' y' else c

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

  (* element sets *)
  module ES = 
  struct

    type t = e list

    let rec compare s1 s2 = 
      match s1, s2 with
	  x::xs, y::ys -> (
	    match E.compare x y with
		x when x = 0 -> compare xs ys
	      | x            -> x
	  )
	| _, []        -> 1
	| [], _        -> -1

    let sort l =
      List.sort compare l

    let contains (l : t) c = List.exists (fun x -> E.compare x c = 0) l

    (* input sets must be ordered! *)
    let merge (s1 : t) (s2 : t) =
      prerr_endline "merging sets";
      let rec merge l s1 s2 =
	match s1,s2 with
	  | [], []         -> List.rev l
	  | [], ys         -> List.append (List.rev l) ys
	  | xs, []         -> List.append (List.rev l) xs
	  | x::xs, y::ys   -> 
	    match E.compare x y with
		c when c = 0 -> merge (x::l) xs ys
	      | c when c < 0 -> merge (x::l) xs (y::ys)
	      | _            -> merge (y::l) (x::xs) ys
      in merge [] s1 s2


    let intersect (s1 : t) (s2 : t) : t =
      prerr_endline "intersecting sets";
      let rec intersect l s1 s2 =
	match s1,s2 with
	  | [], _          -> l
	  |  _, []         -> l
	  | x::xs, y::ys   -> 
	    match E.compare x y with
		c when c = 0 -> intersect (x::l) xs ys
	      | c when c < 0 -> intersect l xs (y::ys)
	      | _            -> intersect l (x::xs) ys
      in intersect [] s1 s2

    let substract (s1 : t) (s2 : t) : t =
      prerr_endline "substracting sets";
      let rec substract l s1 s2 = 
	match s1, s2 with
	  | [], _          -> l
	  |  _, []         -> l
	  | x::xs, y::ys   -> 
	    match E.compare x y with
		0            -> substract l xs ys
	      | c when c < 0 -> substract (x::l) xs (y::ys)
	      | _            -> substract l (x::xs) ys
      in substract [] s1 s2

(*
    (* n way merge *)
    let nmerge l =
      List.fold_left (fun acc x -> merge [] acc x) [] l

    (* n way intersection *)
    let nintersect l : t option =
      let rec nintersect l r =
	match l with
	  | []    -> r
	  | x::xs -> (
	    match intersect x r with
	      | []     -> []
	      | r      -> nintersect xs r
	  )
      in match l with
	| x::xs -> nintersect xs x
	| []    -> []
*)

  end

  (* operations on ranges and sets *)

  (* intersecting ranges and sets *)
  let range_set_intersect (r1,r2) s =
    prerr_endline "intersecting range + set";
    let rec intersect s l = 
    match s with
	[]    -> l
      | x::xs -> 
	if x < r1 then intersect xs l
	else if x > r2 then l
	else intersect xs (x::l)
    in
    Set (List.rev (intersect s []))

  let range_set_merge (r1,r2) s : t list = 
    prerr_endline "merging range + set";
    let rec merge s l =
      match s with 
	| []    -> List.rev l
	| x::xs -> 
	  if x < r1 then merge xs (x::l)
	  else if x > r2 then List.append (List.rev l) s
	  else merge xs l
    in
    match merge s [] with
      | [] -> [ Range (r1,r2) ]  (* the range completely absorbed the set *)
      | l  -> [ Set l ; Range (r1,r2) ]

  (* substracting a set from a range => range list *)
  let range_set_substract r s =
    prerr_endline "substracting set of range";
    let rec substract l (a,b) s =
      match s with
	| []    -> (a,b)::l
	| x::xs -> 
	    match E.compare x a with
	      |	0            -> (
		if E.succ a = b then l else substract l (E.succ a,b) s
	      )
	      | c when c > 0 -> (
		let l' = (a,E.pred x)::l in
		if x = b then l' else substract l' (E.succ x,b) xs
	      )
	      | _            -> substract l (a,b) xs
    in substract [] r s

  (* exported symbols ---------------------------------------- *)

  let compare = Pervasives.compare

  let intersect s1 s2 = 
    match s1, s2 with
      |	Range r1, Range r2 -> (
	match ER.intersect r1 r2 with
	  | Some r -> Range r 
	  | None -> Set []
      )
      | Set s1, Set s2     -> Set (ES.intersect s1 s2)
      | Range r1, Set s1
      | Set s1, Range r1   -> range_set_intersect r1 s1

  let merge s1 s2  =
    match s1, s2 with
      |	Range r1, Range r2 -> (
	(* if the range are disjoint, we can't merge them ! *)
	match ER.merge r1 r2 with 
	  | Some r -> [ Range r ] 
	  | None   -> [ s1; s2 ]
      )
      | Set s1, Set s2     -> [ Set (ES.merge s1 s2) ]
      | Range r1, Set s1
      | Set s1, Range r1   -> range_set_merge r1 s1
	
end
