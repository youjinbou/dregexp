(* Sets with successor and predecessor operators
   elements can be enumerated using these operators
   thus we can use ellipses to handle ranges of elements
*)

module type ELT = 
sig

  type t

  val succ    : t -> t
  val pred    : t -> t

  val compare : t -> t -> int
    
  val print   : t -> unit

end

module type S =
sig

  type e
  type t

  val empty     : t
  val singleton : e -> t
  val range     : e -> e -> t

  val contains  : t -> e -> bool

  val compare   : t -> t -> int

  val intersect : t -> t -> t
  val merge     : t -> t -> t
  val substract : t -> t -> t

  val iter : (e -> unit) -> t -> unit

  val first : t -> e

  val pprint  : t -> unit

end

module Make(E : ELT) : S with type e = E.t and type t = (E.t * E.t) list =
struct

  type 'a pair = 'a * 'a
  type e = E.t

  (* sets as a list of element ranges *)
  type t = e pair list

  let make a b = 
    match E.compare a b with
	x when x < 0  -> (a,b)
      | _             -> (b,a)


  let print_range (a,b) = 
    if a = b 
    then E.print a
    else (E.print a;print_string "-";E.print b)

  let rec pprint = function
    |  []   -> ()
    | x::xs -> (
      print_range x;
      pprint xs
    )

  (* comparison operators for elements *)
  let ( < ) a b  = E.compare a b < 0
  let ( = ) a b  = E.compare a b = 0
  let ( > ) a b  = E.compare a b > 0
  let ( <= ) a b = E.compare a b <= 0
  let ( >= ) a b = E.compare a b >= 0

  (* signature symbols *----------------------------------------*)

  let empty = []

  (* create a set out of a range of elements *)
  let range a b = [ make a b ]

  let singleton a = [ make a a ]

  let contains l e =
    let rec contains e = function
      | []        -> false
      | (a,b)::xs -> not (e < a) && (e <= b || contains e xs)
    in
    contains e l

  let first = function
    | []        -> invalid_arg "Rangeset.first : empty set"
    | (a,b)::_  -> a

  let rec last = function
    | []        -> invalid_arg "Rangeset.last : empty set"
    | [a,b]     -> b
    | x::xs     -> last xs

  (* comparison function using the provided element comparison operator *)
  let compare a b = 
    let pcomp (x1,x2) (y1,y2) =
      match E.compare x1 y1 with
	  0  -> E.compare x2 y2
	| v  -> v
    in
    match a,b with
      | [], []       -> 0
      | [], _        -> -1
      | _, []        -> 1
      | x::xs, y::ys -> match 
	  pcomp x y with
	      0 -> compare xs ys
	    | v -> v

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
	  else if a1 <= b1 && a2 <= b2  (* a1...b1...a2...b2 => b1...a2 *)
	  then intersect xs l2 ((b1,a2)::r)
	  else if b1 <= a1 && a2 <= b2  (* b1...a1...b2...a2 => a1...b2 *)
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
(*
	  print_range (a1,a2);
	  print_string " - ";
	  print_range (b1,b2);
	  print_newline ();
*)
	  if a2 < b1                    (* a1...a2...b1...b2 => a1..a2, keep b1..b2  *)
	  then
	    if E.succ a2 = b1           (* a1...a2b1...b2 => a1..b2 *)
	    then merge xs ((a1,b2)::ys) r
	    else merge xs l2 ((a1,a2)::r)
	  else if b2 < a1               (* b1...b2...a1...a2 => b1..b2, keep a1..a2  *)
	  then
	    if E.succ b2 = a1           (* b1...b2a1...a2 => b1..a2 *)
	    then merge ((b1,a2)::xs) ys r
	    else merge l1 ys ((b1,b2)::r)
	  else if a1 <= b1 && a2 <= b2  (* a1...b1...a2...b2 => a1...b2 *)
	  then merge ((a1,b2)::xs) ys r
	  else if b1 <= a1 && a2 <= b2  (* b1...a1...b2...a2 => b1...a2 *)
	  then merge ((b1,a2)::xs) ys r
	  else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => a1...a2 *)
	  then merge ((a1,a2)::xs) ys r
	  else                          (* b1...a1...a2...b2 => b1...b2 *)
	    merge ((b1,b2)::xs) ys r
	)
    in merge l1 l2 []


  let substract (l1 : t) (l2 : t) : t =
(*
    print_string "substracting: ";
    pprint l1;
    print_string " - ";
    pprint l2;
    print_newline ();
    let print_ranges a b c d =
      E.print a;print_string "...";
      E.print b;print_string "...";
      E.print c;print_string "...";
      E.print d;print_newline ()
    in
*)
    let rec substract l1 l2 r = 
      match l1, l2 with
	| [], _                    -> List.rev r
	| lr, []                   -> List.append (List.rev r) lr
	| (a1,a2)::xs, (b1,b2)::ys -> (
	  if a2 < b1                    (* a1...a2...b1...b2 => [a1..a2] *)
	  then 
	    substract xs l2 ((a1,a2)::r)
	  else if b2 < a1               (* b1...b2...a1...a2 => [a1..a2] *)
	  then
	    substract l1 ys r
	  else if a1 <= b1 && a2 <= b2  (* a1...b1...a2...b2 => [a1...b1[ *)
	  then
	    let x = E.pred b1 in
	    if x < a1
	    then substract xs l2 r
	    else substract xs l2 ((a1,x)::r)
	  else if b1 <= a1 && b2 <= a2  (* b1...a1...b2...a2 => ]b2...a2] *)
	  then
	    let x = E.succ b2 in
	    if x > a2
	    then substract xs ys r
	    else substract ((x, a2)::xs) ys r
	  else if a1 <= b1 && b2 <= a2  (* a1...b1...b2...a2 => [a1..b1[ + ]b2..a2] *)
	  then
	    let x = E.pred b1 and y = E.succ b2 in
	    if x < a1
	    then 
	      if y > a2
	      then substract xs ys r
	      else substract xs ys ((y,a2)::r)
	    else
	      if y > a2
	      then substract xs ys ((a1,x)::r)
	      else substract xs ys ((y,a2)::(a1,x)::r)
	  else
	    substract xs l2 r        (* b1...a1...a2...b2 => [] *)
	) in
    substract l1 l2 []


  let iter (f : e -> unit) (s : t) =
    let rec range_iter f a b =
       if a >= b then f b
       else (f a ; range_iter f (E.succ a) b)
    in
    let rec iter f = function 
      | []        -> ()
      | (a,b)::xs -> range_iter f a b; iter f xs
    in iter f s


end
