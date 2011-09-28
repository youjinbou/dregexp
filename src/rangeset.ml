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

  (* sets as a list of sequences *)
  type t = e pair list

  let make a b = 
    match E.compare a b with
	x when x < 0  -> (a,b)
      | _             -> (b,a)


  let print_range (a,b) = 
      print_string "["; 
      E.print a;
      print_string ".."; 
      E.print b;
      print_string "]"

  (* comparison operators for elements *)
  let (<) a b = E.compare a b < 0
  let (=) a b  = E.compare a b = 0
  let (>) a b = E.compare a b > 0
  let (<=) a b = E.compare a b <= 0
  let (>=) a b = E.compare a b >= 0

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
	  print_range (a1,a2);
	  print_string " - ";
	  print_range (b1,b2);
	  print_newline ();
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


  let iter (f : e -> unit) (s : t) =
    let rec range_iter f a b =
       if a >= b then f b
       else (f a ; range_iter f (E.succ a) b)
    in
    let rec iter f = function 
      | []        -> ()
      | (a,b)::xs -> range_iter f a b; iter f xs
    in iter f s

  let first = function
    | []       -> invalid_arg "first"
    | (a,b)::_ -> a   

  let rec pprint = function
    |  []   -> ()
    | x::xs -> (
      print_range x;
      pprint xs
    )

end

module Test =
struct


  module Char = 
  struct 

    type t = char
    let succ x = Char.chr (succ (Char.code x))
    let pred x = Char.chr (pred (Char.code x))
    let compare = Pervasives.compare 

    let print = print_char

  end

  module CR = Make(Char)

  let v1 = CR.range 'a' 'c' 

  let v2 = CR.range 'e' 'h'

  let v3 = CR.range 'i' 'g'

  let v4 = CR.range 'm' 'q'

  let v5 = CR.range 'w' 'y'

  let v6 = CR.intersect v1 v2

  let v7 = CR.merge (CR.merge v1 v2) (CR.merge v5 v6)

  let v8 = CR.merge (CR.merge v3 v5) v1

  let v9 = CR.intersect v7 v8

  let v10 = CR.merge v2 v3



  let s1 = CR.singleton 'f'
  let s2 = CR.singleton 't'
  let s3 = CR.singleton 'o'

  let r1 = CR.merge v4 s1
  let r2 = CR.merge v2 s2
  let r3 = CR.merge v1 s3

  let r4 = CR.substract v7 s1

  let r5 = CR.substract (CR.merge (CR.merge v1 v2) (CR.merge v3 v4)) r4

  let print name x = 
    print_string (name^" : ");
    CR.pprint x;
    print_newline ()

  let _ = 
    List.iter (fun (x,y) -> print x y) [
      "v1", v1;
      "v2", v2;
      "v3", v3;
      "v4", v4;
      "v5", v5;
      "v6 = v1 & v2", v6;
      "v7 = v1 + v2 + v5 + v6", v7;
      "v8 = v3 + v5 + v1", v8;
      "v9 = v7 & v8 = v1 + v5 + v2 & v3", v9;
      "v10 = v2 + v3", v10;
      "s1", s1;
      "s2", s2;
      "s3", s3;
      "r1 = v4 + s1", r1;
      "r2 = v2 + s2", r2;
      "r3 = v1 + s3", r3;
      "r4 = v7 - s1", r4;
      "r5 = v1 + v2 + v3 + v4 - r4", r5;
    ]

end
