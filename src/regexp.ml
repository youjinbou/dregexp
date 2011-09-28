(*

  implementation of a Regular Expression DFA constructor from 
  RE derivatives.

  Papers :

  [1] Derivatives of regular expressions (Brzozowski)

  [2] Regular expression derivatives reexamined (Owens,Reppy,Turron)

*)

(* the char type 
   - must support pred and succ operators
   - must provide at least one value (any)
*)

module type CHAR = 
sig

  type t

  val any     : t
    
  val succ    : t -> t
  val pred    : t -> t

  val compare : t -> t -> int

  val print  : t -> unit

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

  type 'a pair = 'a * 'a

  open S

  module CS = Rangeset.Make(C)

  (* regexp language def *)
  type t = 
      Epsilon                 (* empty string    *)
    | Any                     (* universal match => could be implemented using Not Empty Set *)
    | EmptySet                
    | Set      of CS.t        (* non empty char set *)
    | Concat   of t pair      (* r . s           *)
    | Kleene   of t           (* r*              *)
    | Or       of t pair      (* r + s           *)
    | And      of t pair      (* r & s           *)
    | Not      of t           (* ...             *)

  let compare (a: t) (b: t) = Pervasives.compare a b

  (* -------------------------------------- *)

  (* delta function - v (nu) in [2] *)
  let rec delta r = 
    match r with
	EmptySet      -> EmptySet
      | Epsilon       -> Epsilon
      | Any           -> EmptySet
      | Set s         -> EmptySet
      | And (a,b)
      | Concat (a,b)  -> (
	match delta a, delta b with
	    EmptySet, _   -> EmptySet
	  | _, EmptySet   -> EmptySet
	  | _, _          -> Epsilon
      )
      | Kleene a      -> Epsilon
      | Or (a,b)      -> (
	match delta a, delta b with
	    Epsilon, _ -> Epsilon
	  | _, Epsilon -> Epsilon
	  | _, _       -> EmptySet
      )
      | Not (a)       -> (
	match delta a with
	    EmptySet        -> Epsilon
	  | _              -> EmptySet
      )

  let nullable r = delta r = Epsilon

  let intersect s1 s2 = 
    let s = CS.intersect s1 s2 in
    if s = CS.empty then EmptySet else Set s

  (* re pseudo constructors, which apply similarity rules reduction *)
  let concat a b = 
    match a, b with
	EmptySet, _
      | _, EmptySet      -> EmptySet 
      | Epsilon, _       -> b
      | _, Epsilon       -> a
      | Concat(a,b), c   -> Concat (a, Concat (b,c))
      | _, _             -> Concat (a, b)
	
  let kleene = function
    | EmptySet     -> Epsilon
    | Kleene a 
    | a            -> Kleene a

  let and_op a b =
    match a, b with
	EmptySet, _ 
      | _, EmptySet        -> EmptySet
      | Any, _             -> b
      | _, Any             -> a
      | Set s1, Set s2     -> intersect s1 s2
      | a,b when a = b     -> a
      | And(a,b),c         -> And (a, And (b,c))
      | _, _               -> And (a,b)

  let or_op a b = 
    match a, b with
      | EmptySet, EmptySet -> EmptySet
      | Any, _
      | _, Any             -> Any
      | EmptySet, _        -> b
      | _, EmptySet        -> a
      | Set s1, Set s2     -> Set (CS.merge s1 s2)
      | a,b when a = b     -> a
      | Or(a,b),c          -> Or (a, Or (b,c))
      | _, _               -> Or (a,b)

  let not_op = function
    | Any      -> EmptySet
    | EmptySet -> Any
    | Not x    -> x
    | a        -> Not a

  (* compute the regexp derivation - d (delta) in [2] *)
  let rec derive c r = 
    match r with
	EmptySet              -> EmptySet
      | Epsilon               -> EmptySet
      | Any                   -> Epsilon
      | Set a when
	  CS.contains a c     -> Epsilon
      | Set a                 -> EmptySet
      | Concat (a,b)          ->
	or_op (concat (derive c a) b) (concat (delta a) (derive c b))
      | Kleene a              -> concat (derive c a) (kleene a)
      | And (a,b)             -> and_op (derive c a) (derive c b)
      | Or  (a,b)             -> or_op (derive c a) (derive c b)
      | Not a                 -> not_op (derive c a)


  (* check whether s is parsed by re *)
  let string_match re s : bool = 
    let rec match_re re s i m =
      if i >= m then nullable re (* we're in an accepting state if the current re accepts Epsilon *)
      else
	match derive (S.get s i) re with
	    EmptySet    -> false
	  | d           -> match_re d s (succ i) m
    in
    match_re re s 0 (S.length s)

  (* Character Set Classes construction *)
  module Class =
  struct

    type re = t

    (* add an element to a list - no duplicate *)
    let rec add compare l c = 
      match l with
	| []    -> [c]
	| x::xs -> match compare x c with
	    | -1 -> x::(add compare xs c)
	    |  0 -> l
	    |  _ -> c::l

    (* n² combinations of f on l1 l2 elements without duplicate *)
    let mapsqr compare (f: 'a -> 'b -> 'c)  (l1 : 'a list) (l2 : 'b list) : 'c list =
      let map_one (f: 'a -> 'b -> 'c) (a : 'a) (l : 'b list) : 'c list = 
	List.map (fun x -> f a x) l 
      in
      let f1 (acc : 'c list) (x : 'a) = 
	List.fold_left (fun acc x -> add compare acc x) acc (map_one f x l2)
      in
      List.fold_left f1 [] l1

    let compare a b = Pervasives.compare a b
      
    type expr = 
	CSet of CS.t
      | CInvSet of CS.t

    type t = expr
       
    let empty = CSet CS.empty

    (* merge class lists, no combination *)
    let rec merge l1 l2 = 
      match l2 with
	  []    -> l1
	| x::xs -> merge (add compare l1 x) xs
	  
    let inter (x : expr) (y : expr) : expr =
      match x,y with
	  CSet s1, CSet s2       -> CSet (CS.intersect s1 s2)
	| CInvSet s1, CInvSet s2 -> CInvSet (CS.merge s1 s2)
	| CSet s2, CInvSet s1
	| CInvSet s1, CSet s2    -> CSet (CS.substract s2 s1)

    let intersect (l1 : expr list) (l2 : expr list) : expr list =
      mapsqr compare inter l1 l2

    let make (re : re) : expr list =
      let rec make = function
	| EmptySet            -> [ CSet CS.empty ]
	| Epsilon             -> [ CInvSet CS.empty ]
	| Any                 -> [ CSet CS.empty; CInvSet CS.empty ]
	| Set   s             -> [ CSet s ; CInvSet s ]
	| Concat (c1,c2) when
	    not (nullable c1) -> make c1
	| Concat (c1,c2)
	| And    (c1,c2)      -> intersect (make c1) (make c2)
	| Or     (c1,c2)      -> intersect (make c1) (make c2)
	| Not     n
	| Kleene  n           -> make n
      in make re

    let sample c =
      match c with
	| CSet s    -> (
	  if CS.empty = s 
	  then None               (* Empty Set *)
	  else Some (CS.first s)  (* first of the set *)
	)
	| CInvSet s -> (
	  if CS.empty = s 
	  then                    (* any will do *)
	    Some (C.any)
	  else                    (* we need to find a char which is not in s *)
	    Some (C.pred (CS.first s))
	)

  end

  module DFA = 
  struct

    type re = t

    (* a transition is an arrow from one state to another labeled with a derivative 
       class. We have a limited number of charsets for each state, and a limited 
       number of states, each being produced by deriving a regexp wrt a class. So 
       we explore the DFA graph by 
       1 - building the set of derivative classes accepted by the regexp at the current
           state,
       2 - deriving this regexp against each class in turn, and see if we discover a
           new state
       (see [2], Chapter 3 & 4)
    *)

    (* Deterministic Finite Automaton type *)
    type t = {
      classes       : Class.t array;     (* character class array for each state *)
      transitions   : int array array;   (* transition from (state, char set) to state *)
    }

    (* the DFA construction machinery *)
    module Builder =
    struct

      exception Error of String.t


      (* RegExp Derivatives Mapping *)
      module QSet = Map.Make(struct type t = re let compare = compare end)

      module ClassSet = Map.Make(struct type t = Class.t let compare = Class.compare end)

      module AB = VecBuffer.ArrayBuffer

      type 'a abuffer = 'a AB.t

      type state = int
      type cs    = int
	  
      type t = {
	mutable b_ccount  : int;                           (* class count *)
	mutable b_states  : state QSet.t;                  (* derivative mapping to states *)
	mutable b_classes : cs ClassSet.t;                 (* derivative classes set *)
	b_transitions     : (cs * state) abuffer abuffer;  (* transitions *)
      }
    
      (* dfa states ------------------- *)

      let state_count (b : t) : int = AB.length b.b_transitions

      let add_state (b : t) (qc : re) : state = 
	let count = AB.add b.b_transitions (AB.create 0 (-1,-1)) in
	b.b_states  <- QSet.add qc count b.b_states;
	count

      let find_state (b : t) (qc : re) : state = QSet.find qc b.b_states

      let register_state (b : t) (qc : re) : bool * state =
	try
	  false, find_state b qc
	with Not_found -> true, add_state b qc

      (* input character class sets --- *)

      let class_count (b : t) : int = b.b_ccount

      let find_class (b : t) (s : Class.t) : cs = ClassSet.find s b.b_classes

      let add_class (b : t) (s : Class.t) : cs =
    	b.b_ccount  <- succ b.b_ccount;
	b.b_classes <- ClassSet.add s b.b_ccount b.b_classes;
	b.b_ccount
	  
      let register_class (b : t) (s : Class.t) : bool * cs =
	try 
	  false, find_class b s 
	with Not_found -> true, add_class b s

      (* dfa state transitions---------- *)

      let find_transition (b : t) (source : state) (cs : cs) : state =
	let v = AB.nth b.b_transitions source in
	let rec find v cs i m =
	  if (i >= m)
	  then raise Not_found
	  else
	    let (cs', target) = AB.nth v i in
	    if cs' = cs then target
	   else find v cs (succ i) m
	in
	find v cs 0 (AB.length v)
	  
      let add_transition (b : t) (source : state) (cs : cs) (target : state) = 
	AB.add (AB.nth b.b_transitions source) (cs, target)
	  
      let register_transition (b : t) (source : state) (cs : cs) (target : state) : unit =
	try 
	  if (find_transition b source cs = target)
	  then raise (Error "invalid transition : trying to create a transition from state and character class leading to a different state");
	with Not_found -> ignore (add_transition b source cs target)

      (* dfa construction algorithm  --- *) 

      let rec goto (q : re) (b : t) (set : Class.t) : t =
	match (Class.sample set) with
	    None   -> b
	  | Some c ->
	    let qc = derive c q in
	    let n, target = register_state b qc
	    and source = find_state b q
	    and _, cs = register_class b set in
	    register_transition b source cs target;
	    if n then explore qc b else b

      and explore (q : re) (b: t) : t = 
	List.fold_left (fun acc x -> goto q acc x) b (Class.make q)
	
      let make re = 
	let b = {
	  b_ccount       = 0;               
	  b_states       = QSet.add re 0 QSet.empty;
	  b_classes      = ClassSet.empty;  
	  b_transitions  = AB.create 1 (AB.create 0 (-1,-1));
	} in explore re b

      (* produce the DFA *)
      let build (b : t) = 
	let fillup_state v ab =
	  for i = 0 to AB.length ab do
	    let cs, t = AB.nth ab i in
	    v.(cs) <- t
	  done
	in
	let make_state_transition b i = 
	  let v = Array.make (class_count b) (-1) in
	  fillup_state v (AB.nth b.b_transitions i);
	  v
	in
	let c = Array.make (class_count b) Class.empty in 
	ClassSet.iter (fun cs i -> c.(i) <- cs) b.b_classes;
	{
	classes     = c;
	transitions = Array.init (state_count b) (make_state_transition b);
      }
	  
    end (* Builder *)

    let make re = 
      Builder.build (Builder.make re)
      
  end  (* DFA *)

end
