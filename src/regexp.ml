(*

  implementation of a Regular Expression DFA constructor from 
  RE derivatives.

  Papers :

  [1] Derivatives of regular expressions (Brzozowski)

  [2] Regular expression derivatives reexamined (Owens,Reppy,Turron)

*)

module type CHAR = 
sig

  type t
    
(*
  val code    : t -> int
  val chr     : int -> t
*)
  val succ    : t -> t
  val pred    : t -> t

  val compare : t -> t -> int

end

module type STRING_SIMPLE =
sig

  module Char : CHAR

  type t

  val make     : int -> Char.t -> t
  val length   : t -> int
  val sub      : t -> int -> int -> t
  val get      : t -> int -> Char.t
  val set      : t -> int -> Char.t -> unit
  val compare  : t -> t   -> int

end

(* provides a substract function *)
module Extend (S : STRING_SIMPLE) =
struct

  include S
    
  (* substract s2 to s1 *)
  let substract s1 s2 = 
    let l1 = length s1 
    and l2 = length s2 
    in
    let rec ib i =
      if i = -1 
      then Some (sub s1 l2 (l2 - l1))
      else
	if (get s1 i) = (get s2 i) then ib (pred i)
	else None
    in ib (pred l2)

end

module type STRING =
sig

  module Char : CHAR

  type t

  val make      : int -> Char.t -> t
  val length    : t -> int
  val sub       : t -> int -> int -> t
  val get       : t -> int -> Char.t
  val set       : t -> int -> Char.t -> unit
  val compare   : t -> t   -> int
  val substract : t -> t   -> t option

end

(* regexp over the string S.t *)
module Make (S : STRING) =
struct

  module C = S.Char

  type string = S.t

  type char = S.Char.t

  open S

  type 'a pair = 'a * 'a

  type set   = char list

  type range = char pair

  (* regexp language def *)
  type t = 
      Epsilon                 (* empty string    *)
    | Any                     (* universal match *)
    | Set      of set         (* char set        *)
    | Range    of range       (* char range      *)
    | Concat   of t pair      (* r . s           *)
    | Kleene   of t           (* r*              *)
    | Or       of t pair      (* r + s           *)
    | And      of t pair      (* r & s           *)
    | Not      of t           (* ...             *)

  (* char ranges *)
  module CR =
  struct

    let make a b = 
      match C.compare a b with
	  x when x < 0  -> Range (a,b)
	| x when x > 0  -> Range (b,a)
	| _             -> Set [a]

    let compare (x,x') (y,y') = 
      let c = C.compare x y in
      if c = 0 then C.compare x' y' else c

    let sort l =
      List.sort compare l

    let intersect (a1,a2) (b1,b2) =
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

    let merge (a1,a2) (b1,b2) =
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

    let substract (a1,a2) (b1,b2) =
      prerr_endline "substracting ranges";
      if a2 < b1 || b2 < a1         (* disjoint ranges *)
      then [ (a1,a2) ]
      else if a1 <= b1 && a2 >= b1  (* a1...b1...a2...b2 => [ a1...b1 [ *)
      then let x = C.pred b1 in if x <= a1 then [] else [ (a1,x) ]
      else if b1 <= a1 && b2 >= a1  (* b1...a1...b2...a2 => ] b2...a2 ] *)
      then let x = C.succ b2 in if x >= a2 then [] else [ (x, a2) ]
      else if a1 <= b1 && a2 >= b2  (* a1...b1...b2...a2 => [ a1..b1 [ ; ] b2 .. a2 ] *)
      then
	let x = C.pred b1 and y = C.succ b2 in
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

  (* char sets *)
  module CS = 
  struct

    type t = set

    let rec compare s1 s2 = 
      match s1, s2 with
	  x::xs, y::ys -> (
	    match C.compare x y with
		x when x = 0 -> compare xs ys
	      | x            -> x
	  )
	| _, []        -> 1
	| [], _        -> -1

    let sort l =
      List.sort compare l


    let contains (l : set) c = List.exists (fun x -> C.compare x c = 0) l

    (* input sets must be ordered! *)
    let merge (s1 : set) (s2 : set) =
      prerr_endline "merging sets";
      let rec merge l s1 s2 =
	match s1,s2 with
	  | [], []         -> List.rev l
	  | [], ys         -> List.append (List.rev l) ys
	  | xs, []         -> List.append (List.rev l) xs
	  | x::xs, y::ys   -> 
	    match C.compare x y with
		c when c = 0 -> merge (x::l) xs ys
	      | c when c < 0 -> merge (x::l) xs (y::ys)
	      | _            -> merge (y::l) (x::xs) ys
      in merge [] s1 s2


    let intersect (s1 : set) (s2 : set) =
      prerr_endline "intersecting sets";
      let rec intersect l s1 s2 =
	match s1,s2 with
	  | [], _          -> l
	  |  _, []         -> l
	  | x::xs, y::ys   -> 
	    match C.compare x y with
		c when c = 0 -> intersect (x::l) xs ys
	      | c when c < 0 -> intersect l xs (y::ys)
	      | _            -> intersect l (x::xs) ys
      in intersect [] s1 s2

    let substract (s1 : set) (s2 : set) : set =
      prerr_endline "substracting sets";
      let rec substract l s1 s2 = 
	match s1, s2 with
	  | [], _          -> l
	  |  _, []         -> l
	  | x::xs, y::ys   -> 
	    match C.compare x y with
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

  let range_set_merge (r1,r2) s = 
    prerr_endline "merging range + set";
    let rec merge s l =
      match s with 
	| []    -> List.rev l
	| x::xs -> 
	  if x < r1 then merge xs (x::l)
	  else if x > r2 then List.append (List.rev l) s
	  else merge xs l
    in
    Set (merge s []), Range (r1,r2)

  (* substracting a set from a range => range list *)
  let range_set_substract r s =
    prerr_endline "substracting set of range";
    let rec substract l (a,b) s =
      match s with
	| []    -> (a,b)::l
	| x::xs -> 
	    match C.compare x a with
	      |	0            -> (
		if C.succ a = b then l else substract l (C.succ a,b) s
	      )
	      | c when c > 0 -> (
		let l' = (a,C.pred x)::l in
		if x = b then l' else substract l' (C.succ x,b) xs
	      )
	      | _            -> substract l (a,b) xs
    in substract [] r s
    

  (* -------------------------------------- *)

  (* delta function - v (nu) in [2] *)
  let rec delta r = 
    match r with
	Set []        -> Set []
      | Epsilon       -> Epsilon
      | Any           -> Set []
      | Set s         -> Set []
      | Range (_, _)  -> Set []
      | And (a,b)
      | Concat (a,b)  -> (
	match delta a, delta b with
	    Set [], _   -> Set []
	  | _, Set []   -> Set []
	  | _, _        -> Epsilon
      )
      | Kleene a      -> Epsilon
      | Or (a,b)      -> (
	match delta a, delta b with
	    Epsilon, _ -> Epsilon
	  | _, Epsilon -> Epsilon
	  | _, _       -> Set []
      )
      | Not (a)       -> (
	match delta a with
	    Set []         -> Epsilon
	  | _              -> Set []
      )

  let nullable r = delta r = Epsilon

  (* re pseudo constructors, which apply similarity rules reduction *)
  let concat a b = 
    match a, b with
	Set [], _
      | _, Set []      -> Set [] 
      | Epsilon, _     -> b
      | _, Epsilon     -> a
      | Concat(a,b), c -> Concat (a, Concat (b,c))
      | _, _           -> Concat (a, b)
	
  let kleene = function
    | Set []     -> Epsilon
    | Kleene a 
    | a          -> Kleene a

  let and_op a b =
    match a, b with
	Set [], _ 
      | _, Set []          -> Set []
      | Any, _             -> b
      | _, Any             -> a
      | Set s1, Set s2     -> Set (CS.intersect s1 s2) 
      | Range r1, Range r2 -> (
	match CR.intersect r1 r2 with
	    Some p -> Range p
	  | None   -> Set []
      )
      | Set s, Range r     -> range_set_intersect r s
      | Range r,Set s      -> range_set_intersect r s
      | a,b when a = b     -> a
      | And(a,b),c         -> And (a, And (b,c))
      | _, _               -> And (a,b)

  let or_op a b = 
    match a, b with
      | Set [], Set []     -> Set []
      | Any, _
      | _, Any             -> Any
      | Set [], _          -> b
      | _, Set []          -> a
      | Set s1, Set s2     -> Set (CS.merge s1 s2)
      | Range r1, Range r2 -> (
	match CR.merge r1 r2 with
	    Some p -> Range p
	  | None   -> Or (Range r1,Range r2)
      )
      | Set s1, Range r1   -> Or (range_set_merge r1 s1)
      | Range r1,Set s1    -> Or (range_set_merge r1 s1)
      | a,b when a = b     -> a
      | Or(a,b),c          -> Or (a, Or (b,c))
      | _, _               -> Or (a,b)

  let not_op = function
    | Any    -> Set []
    | Set [] -> Any
    | Not x  -> x
    | a      -> Not a

  (* compute the regexp derivation - d (delta) in [2] *)
  let rec derive c r = 
    match r with
	Set []                -> Set []
      | Epsilon               -> Set []
      | Any                   -> Epsilon
      | Set a when
	  CS.contains a c     -> Epsilon
      | Set a                 -> Set []
      | Range (r1,r2) when
          r1 <= c && r2 >= c  -> Epsilon
      | Range (_,_)           -> Set []
      | Concat (a,b)          ->
	or_op (concat (derive c a) b) (concat (delta a) (derive c b))
      | Kleene a              -> concat (derive c a) (kleene a)
      | And (a,b)             -> and_op (derive c a) (derive c b)
      | Or  (a,b)             -> or_op (derive c a) (derive c b)
      | Not a                 -> not_op (derive c a)


  (* check whether s is parsed by re *)
  let match_ re s : bool = 
    let rec match_re re s i m =
      if i >= m then nullable re (* we're in an accepting state if the current re accepts Epsilon *)
      else
	match derive (S.get s i) re with
	    Set []    -> false
	  | d         -> match_re d s (succ i) m
    in
    match_re re s 0 (S.length s)


      (*
  (* RE Character equivalence class construction *)
  module CharClass =
  struct

    (* add an element to a list - no duplicate *)
    let rec add compare l c = 
      match l with
	| []    -> [c]
	| x::xs -> match compare x c with
	    | -1 -> x::(add compare xs c)
	    |  _ -> l


    (* n² combinations of f on l1 l2 elements *)
    let mapsqr (f: 'a -> 'b -> 'c)  (l1 : 'a list) (l2 : 'b list) : 'c list =
      let map_one (f: 'a -> 'b -> 'c) (a : 'a) (l : 'b list) : 'c list = 
	List.map (fun x -> f a x) l 
      in
      let f1 (acc : 'c list) (x : 'a) = 
	List.fold_left (fun acc x -> add compare acc x) acc (map_one f x l2)
      in
      List.fold_left f1 [] l1

    type t = 
      | CRange of char pair      (* Ranges  a..b *)
      | NRange of char pair      (* Sigma \ Range a..b *)
      | CSet   of char list      (* Set of char *)
      | NSet   of char list      (* Sigma \ Set of char *)

    let compare c1 c2 =
      match c1, c2 with
	  CRange c1, CRange c2
	| NRange c1, NRange c2 -> CR.compare c1 c2
	| CSet   s1, CSet s2
	| NSet   s1, NSet s2   -> CS.compare s1 s2
	| CSet    _, CRange _
	| CSet    _, NRange _
	| NSet    _, CRange _
	| NSet    _, NRange _
	| CRange  _, NRange _
	| CSet    _, NSet   _  -> 1
	| NRange  _, CSet _
	| CRange  _, NSet _
	| NRange  _, CSet _
	| NRange  _, NSet _
	| NRange  _, CRange _
	| NSet    _, CSet   _  -> -1

    let combine c1 c2 =
      match c1, c2 with
	  CRange c1, CRange c2 -> match CR.intersect c1 c2 with None -> CSet [] | Some r -> [CRange r]
	| NRange c1, NRange c2 -> match CR.merge c1 c2 with None -> [NRange c1 ; NRange c2]
	| CSet   s1, CSet s2   -> CSet (CS.intersect s1 s2)
	| NSet   s1, NSet s2   -> CSet (CS.merge s1 s2)
	| NRange  n, CRange r
	| CRange  r, NRange n  -> CR.substract r n
	| CSet    s, NSet   n
	| NSet    n, CSet   s  -> CS.substract s n
	| CRange s1, CSet c1
	| CSet   s1, CRange c1 -> range_set_intersect c1 s1
	| CSet   s1, NRange c1
	| NRange c1, CSet s1   -> set_range_substract s1 c1
	| NSet   s1, CRange c1
	| CRange c1, NSet s1   -> range_set_substract c1 s1
	| NSet   s1, NRange c1
	| NRange c1, NSet s1   -> range_set_merge c1 s1



    let unique fequal l =
      let rec unique l r = 
	match l with
	  | []        -> r
	  | x::[]     -> x::r
	  | x::x'::xs -> 
	    if fequal x x'
	    then unique (x'::xs) r
	    else unique (x'::xs) (x::r)
      in 
      unique l []

    let partition l = 
      let rec partition l cr nr cs ns =
	match l with
	  | []    -> cr,nr,cs,ns
	  | x::xs ->
	    match x with
	      |	CRange p -> partition xs (p::cr) nr cs ns
	      | NRange p -> partition xs cr (p::nr) cs ns
	      | CSet s   -> partition xs cr nr (s::cs) ns
	      | NSet s   -> partition xs cr nr cs (s::ns)
      in partition l [] [] [] []

    (* intersections and merges of csets by type *)
    let intersect c1 c2 =
      let cr, nr, cs, ns = partition (List.append c1 c2) in
      List.concat [
	List.map (fun r -> CRange r) (unique CR.sort (fun x y -> CR.compare x y = 0) cr);
	List.map (fun r -> NRange r) (CR.nmerge nr);
	List.map (fun s -> CSet s) (unique CS.sort (fun x y -> CS.compare x y = 0) cs);
	[ NSet (CS.nmerge ns) ]
      ]

    (* build the character classes out of an re *)
    let make re = 
      let rec make = function
	| Epsilon             -> [ NSet [] ]
	| Any                 -> [ NSet [] ; CSet [] ]
	| Set   s             -> [ CSet s ; NSet s ]
	| Range  (r1,r2)      -> [ CRange (r1,r2) ; NRange (r1,r2) ]
	| Concat (c1,c2) when
	    delta c1 = Set [] -> make c2
	| Concat (c1,c2)
	| Or     (c1,c2)
	| And    (c1,c2)      -> nintersect (make c1) (make c2)
	| Not    (n)
	| Kleene  n           -> make n
      in
      merge (make re) 

  end
      *)

  module Class =
  struct

    type re = t

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
    
    type expr = 
	CSet of char list
      | CRange of char pair
      | CInvSet of char list
      | CInvRange of char pair
      | CInter of expr * expr


    let compare a b = Pervasives.compare a b

    let inter (x : expr) (y : expr) : expr =
      if x < y then  CInter (x,y) else CInter (y,x)

    let intersect (l1 : expr list) (l2 : expr list) : expr list =
      mapsqr compare inter l1 l2

(*
    let rec compile = 
      
      let intersect c1 c2 = 
	match c1, c2 with
	  | CSet s1, CSet s2     -> Set (CS.intersect s1 s2)
	  | CRange r1, CRange r2 -> (
	    match CR.intersect r1 r2 with 
		None   -> Set []
	      | Some r -> Range r
	  )
	  | CRange r, CSet s
	  | CSet r, CRange s    -> range_set_intersect r s
	  | _, _                -> invalid_arg "compile.intersect"
	    
      and merge c1 c2 = 
	match c1, c2 with
	  | CSet s1, CSet s2     -> Set (CS.merge s1 s2)
	  | CRange r1, CRange r2 -> (
	    match CR.merge r1 r2 with 
		None   -> 
	      | Some r -> Range r
	  )
	  | CRange r, CSet s
	  | CSet r, CRange s    -> range_set_intersect r s
	  | _, _                -> invalid_arg "compile.intersect"
      in

	function
      | CInv   e     -> inverse (compile e)
      | CUnion e1,e2 -> merge (compile e1) (compile e2)
      | CInter e1,e2 -> intersect (compile e1) (compile e2)
      | x            -> x
*)

    let make (re : re) : expr list =
      let rec make = function
	| Epsilon             -> [ CInvSet [] ]
	| Any                 -> [ CInvSet [] ; CSet [] ]
	| Set   s             -> [ CSet s ; CInvSet s ]
	| Range  (r1,r2)      -> [ CRange (r1,r2) ; CInvRange (r1,r2) ]
	| Concat (c1,c2) when
	    not (nullable c1) -> make c2
	| Concat (c1,c2)
	| Or     (c1,c2)
	| And    (c1,c2)      -> intersect (make c1) (make c2)
	| Not    (n)
	| Kleene  n           -> make n
      in make re


  end


  module DFA = 
  struct

    (* a transition is an arrow from one state to another labeled with a charset 
       we have a limited number of charsets, known in advance, and a limited number
       of states. => we label an arrow with the charset rank. We label a state with
       the re derivative it matches.
       The transition is thus : (charset * state) -> state = (int * int) -> int
    *)

    type state = int

    module Transition = Map.Make(struct type t = int * int let compare = Pervasives.compare end)

      (*

    type re = t 

    type t = 

    let rec goto q sset qset delta =

    and explore qset delta q =

    let make r =
      let q0 = r
      and qset, delta = explore 

      *)

  end

end






