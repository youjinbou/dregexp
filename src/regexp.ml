(**************************************************************************)
(*                                                                        *)
(*      A regular expression handling library                             *)
(*                                                                        *)
(*      Copyright (C) 2011  Didier Cassirame                              *)
(*                                                                        *)
(*      This library is free software;  you can  redistribute it and/or   *)
(*      modify  it  under the terms  of the  GNU  Lesser General Public   *)
(*      License  as published by  the Free Software Foundation;  either   *)
(*      version 3 of the License, or (at your option) any later version.  *)
(*                                                                        *)
(*      This library is distributed in the hope that it will be useful,   *)
(*      but WITHOUT ANY WARRANTY;  without even the implied warranty of   *)
(*      MERCHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the   *)
(*      GNU Lesser General Public License for more details.               *)
(*                                                                        *)
(*      You should have received a copy of the GNU Lesser General Public  *)
(*      License  along  with this library;  if not,  write to  the Free   *)
(*      Software Foundation, Inc.,                                        *)
(*      51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA      *)
(*                                                                        *)
(**************************************************************************)

(*

  Implementation of a Regular Expression DFA constructor from 
  RE derivatives.

  Papers on which this code is based:

  [1] Derivatives of regular expressions (Brzozowski)

  [2] Regular expression derivatives reexamined (Owens,Reppy,Turron)

*)

(* char type 
   - must support pred and succ operators
   - must provide at least one value (any)
*)


module type CHAR =
sig
  
  type t

  val succ : t -> t
  val pred : t -> t

  val any : t
  val first : t
  val last  : t

  val compare : t -> t -> int
  val to_string : t -> string

end

(* minimal string type module signature for regexp *)
module type STRING =
sig

  module Char : CHAR

  type t

  val length   : t -> int
  val get      : t -> int -> Char.t
(*  
  val make     : int -> Char.t -> t
  val sub      : t -> int -> int -> t 
  val set      : t -> int -> Char.t -> unit 
  val compare  : t -> t   -> int
*)

end

(** regexp functor over the string S.t *)
module Make (S : STRING) =
struct

  module C = S.Char

  type string_t = S.t

  type char_t = S.Char.t  

  type 'a pair = 'a * 'a

  module CS = Rangeset.Make(C)

  let sprint s =
    for i = 0 to pred (S.length s) do
      print_string (C.to_string (S.get s i));
    done

  (** sub-regexp annotation *)
  type annot = int option

  (** primitive regexp language definition *)
  type t = 
      Epsilon                   (* empty string       *)
    | Any                       (* universal match    *)
    | EmptySet                  (* empty char set     *)
    | Set      of CS.t          (* non empty char set *)
    | Concat   of t pair        (* r . s              *)
    | Kleene   of t             (* r*                 *)
    | Repeat   of t * int       (* r{0,n}             *) 
    | Or       of t pair        (* r | s              *)
    | And      of t pair        (* r & s              *)
    | Not      of t             (* ...                *)

  (** type alias for submodules *)
  type re = t 

  (** comparison function using the CS comparison operator when needed *)
  let rec compare a b = 
    match a, b with
    | Epsilon , Epsilon 
    | Any     , Any
    | EmptySet, EmptySet             -> 0
    | Set s1, Set s2                 -> CS.compare s1 s2
    | Not    x, Not y 
    | Kleene x, Kleene y             -> compare x y
    | Or     (x1,x2), Or (y1,y2)
    | And    (x1,x2), And (y1,y2)
    | Concat (x1,x2), Concat (y1,y2) -> (
      match compare x1 y1 with
	  0 -> compare x2 y2
	| v -> v
    )
    | Repeat (x,k1), Repeat (y,k2) when k1 = k2 -> compare x y
    | k1, k2 -> 
      (* here we know that the two values CANNOT be equal, so we might as well rely on 
	 the structural comparison, which should apply the same ordering *)
      Pervasives.compare k1 k2

  (** regular expression pretty printer *)
  let rec pprint = 
    let parens f =
      print_string "("; f (); print_string ")" in
    function
    | Epsilon        -> print_string "Îµ"
    | Any            -> print_string "."
    | EmptySet       -> print_string "[]"
    | Set s          -> print_string "[";CS.pprint s; print_string "]"
    | Concat (r1,r2) -> pprint r1; pprint r2
    | Or     (r1,r2) -> pprint r1; print_string "|"; pprint r2
    | And    (r1,r2) -> pprint r1; print_string "&"; pprint r2
    | Kleene  r      -> parens (fun () -> pprint r); print_string "*"
    | Repeat (r,k)   -> parens (fun () -> pprint r);print_string "{"; print_int k; print_string"}"
    | Not     r      -> print_string "!";parens (fun () -> pprint r)

  (* -------------------------------------- *)

  (** delta function - v (nu) in [2] *)
  let rec delta r = 
    match r with
	EmptySet      -> EmptySet
      | Epsilon       -> Epsilon
      | Any           -> EmptySet
      | Set _         -> EmptySet
      | And (a,b)
      | Concat (a,b)  -> (
	match delta a, delta b with
	    EmptySet, _   -> EmptySet
	  | _, EmptySet   -> EmptySet
	  | _, _          -> Epsilon
      )
      | Kleene _      -> Epsilon
      | Repeat (a,x)  -> Epsilon  (* == Epsilon | a | a.a | a.a.a ... => Epsilon *)
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

  (** re pseudo constructors, which apply similarity rules reduction *)
  let concat a b = 
    match a, b with
	EmptySet, _
      | _, EmptySet      -> EmptySet 
      | Epsilon, _       -> b
      | _, Epsilon       -> a
      | Concat(a,b), c   -> Concat (a, Concat (b,c))
      | _, _             -> Concat (a, b)
	
  let kleene = function
    | Epsilon
    | EmptySet     -> Epsilon
    | Kleene a 
    | Repeat (a,_)
    | a            -> Kleene a

  let repeat x = function
    | Epsilon
    | EmptySet     -> Epsilon
    | Kleene a     -> Kleene a
    | a            -> Repeat (a, x)
  
  let and_op a b =
    match a, b with
	EmptySet, _ 
      | _, EmptySet        -> EmptySet
      | Any, Set _         -> b
      | Set _, Any         -> a
      | a,b when
	  compare a b = 0  -> a
      | Set s1, Set s2     -> intersect s1 s2
      | And(a,b),c         -> And (a, And (b,c))
      | _, _               -> And (a,b)

  let or_op a b = 
    match a, b with
      | EmptySet, EmptySet -> EmptySet
      | EmptySet, _        -> b
      | _, EmptySet        -> a
      | Any, Set _
      | Set _, Any         -> Any
      | a,b when 
	  compare a b = 0  -> a
      | Set s1, Set s2     -> Set (CS.merge s1 s2)
      | Or(a,b),c          -> Or (a, Or (b,c))
      | _, _               -> Or (a,b)

  let not_op = function
    | Any      -> EmptySet
    | EmptySet -> Any
    | Not x    -> x
    | a        -> Not a

  (** compute the regexp derivation - d (delta) in [2] *)
  let rec derive c r = 
    match r with
      | Any                   -> Epsilon
      | Set a when
	  CS.contains a c     -> Epsilon
      | EmptySet
      | Epsilon               -> EmptySet
      | Set a                 -> EmptySet
      | Concat (a,b)          ->
	or_op (concat (derive c a) b) (concat (delta a) (derive c b))
      | Kleene a              -> concat (derive c a) (kleene a)
      | Repeat (a,x)          -> (
	match x with
	    0 -> EmptySet 
	  | 1 -> derive c a
	  | _ -> concat (derive c a) (repeat (pred x) a)
      )
      | And (a,b)             -> and_op (derive c a) (derive c b)
      | Or  (a,b)             -> or_op (derive c a) (derive c b)
      | Not a                 -> not_op (derive c a)

  type match_kind = SHORTEST | LONGEST | FIRST

  type match_solution = re * int (* re derivative & string length *)

  (** check whether s is parsed by re *)
  let string_match kind re s : match_solution option = 
    let result der len m =
      if len <= m && len >= 0
      then Some (der,len) 
      else None
    in
    let rec shortest_match_re re s i m (der,len) =
      let der, len = if nullable re && i < len then re, i else der, len in
      if i >= m 
      then result der len m
      else
	match derive (S.get s i) re with
	    EmptySet    -> result der len m
	  | d           -> shortest_match_re d s (succ i) m (der,len)
    and first_match_re re s i m =
      if nullable re 
      then Some (re, i)
      else
	match derive (S.get s i) re with
	    EmptySet    -> None
	  | d           -> first_match_re d s (succ i) m
    and longest_match_re re s i m (der,len) =
      let der, len = if nullable re && i > len then re, i else der, len in
      if i >= m 
      then result der len m
      else
	match derive (S.get s i) re with
	    EmptySet    -> result der len m
	  | d           -> longest_match_re d s (succ i) m (der,len)
    in
    let l = S.length s in
    match kind with
	SHORTEST -> shortest_match_re re s 0 l (re, succ l)
      | LONGEST  -> longest_match_re re s 0 l (re, -1)
      | FIRST    -> first_match_re re s 0 l 

  (** Derivative CharSet Classes construction module *)
  (*
     it's basically an alphabet based (i.e. discrete) set arithmetic augmented with 
     the inverse operator :
       INVERSE(E) = SIGMA - E where SIGMA is the whole alphabet
  *)
  module Class =
  struct

    let pprint_re = pprint

    (* add an element to a sorted list - no duplicate *)
    let rec add comp l c = 
      match l with
	| []    -> [c]
	| x::xs -> match comp x c with
	    | 0            -> l
	    | v when v < 0 -> x::(add comp xs c)
	    | _            -> c::l

    (* n² combinations of f on l1 l2 elements without duplicate *)
    let mapsqr comp (f: 'a -> 'b -> 'c)  (l1 : 'a list) (l2 : 'b list) : 'c list =
      let map_one (f: 'a -> 'b -> 'c) (a : 'a) (l : 'b list) : 'c list = 
	List.map (fun x -> f a x) l 
      in
      let f1 (acc : 'c list) (x : 'a) = 
	List.fold_left (fun acc x -> add comp acc x) acc (map_one f x l2)
      in
      List.fold_left f1 [] l1

    type expr = 
	CSet of CS.t
      | CInvSet of CS.t

    type t = expr


    let contains (cs : t) (c : C.t) =
      match cs with
	  CSet    cs -> CS.contains cs c
	| CInvSet cs -> not (CS.contains cs c)

    let compare a b = 
      match a, b with
	  CSet x, CSet y
	| CInvSet x, CInvSet y  -> CS.compare x y
	| _,_  -> Pervasives.compare a b

    let sort (l : t list) : t list  = List.sort compare l

    let to_string = function
      | CSet s      -> "["^CS.to_string s^"]"
      | CInvSet s   -> "^["^CS.to_string s^"]"
       
    let pprint x = print_string (to_string x)

    let empty = CSet CS.empty

    (* merge class lists, no combination *)
    let rec merge l1 l2 = 
      match l2 with
	  []    -> l1
	| x::xs -> merge (add compare l1 x) xs

    (* this operation simplify the combination of x and y *)
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
	| Any                 -> (* sort *) [ (* CSet CS.empty ; *) CInvSet CS.empty ]
	| Set   s             -> (* sort *) [ CSet s (* ; CInvSet s *) ]
	| Concat (c1,c2) when
	    not (nullable c1) -> make c1
	| Concat (c1,c2)
	| Or     (c1,c2)      -> merge (make c1) (make c2)
	| And    (c1,c2)      -> intersect (make c1) (make c2)
	| Not     n
	| Kleene  n           -> make n
	| Repeat  (n, _)      -> make n
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
	    Some (C.pred (CS.first s)) (* fix-me: hopefully, first of s has a predecessor *)
	)

  end

  module DFA = 
  struct

    let pprint_re = pprint


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
      accept        : bool array;        (* accepting states *)
      transitions   : int array array;   (* transition from (state, char set) to state *)
    }

    (* the DFA construction machinery *)
    module Builder =
    struct

      exception Failure of String.t

      type state = int
      type cs    = int

      (* RegExp Derivatives Mapping *)
      module QSet = Map.Make(struct type t = re let compare = compare end)

      module StateSet = Set.Make(struct type t = cs let compare = Pervasives.compare end)

      module ClassSet = Map.Make(struct type t = Class.t let compare = Class.compare end)

      module AB = VecBuffer.ArrayBuffer

      type 'a abuffer = 'a AB.t

      type dfa = t
	  
      type t = {
	mutable b_ccount  : int;                           (* class count *)
	mutable b_states  : state QSet.t;                  (* derivative mapping to states *)
	mutable b_accept  : StateSet.t;                    (* set of accepting states *)
	mutable b_classes : cs ClassSet.t;                 (* derivative classes set *)
	b_transitions     : (cs * state) abuffer abuffer;  (* transitions *)
      }
    
      (* dfa states ------------------- *)

      let pprint_stateset (b : t) : unit = 
	QSet.iter (fun k v -> pprint_re k; print_string " -> "; print_int v; print_newline ()) b.b_states

      let state_count (b : t) : int = AB.length b.b_transitions

      let add_state (b : t) (qc : re) : state = 
	let count = AB.add b.b_transitions (AB.create 0 (-1,-1)) in
	b.b_states <- QSet.add qc count b.b_states;
	if nullable qc then b.b_accept <- StateSet.add count b.b_accept;
	count

      let find_state (b : t) (qc : re) : state = QSet.find qc b.b_states

      let register_state (b : t) (qc : re) : bool * state =
	try
	  false, find_state b qc
	with Not_found -> true, add_state b qc

      (* input character class sets --- *)

      let pprint_classset (b : t) = 
	ClassSet.iter (fun k v -> Class.pprint k; print_string " -> "; print_int v; print_newline ()) b. b_classes

      let class_count (b : t) : int = b.b_ccount

      let find_class (b : t) (s : Class.t) : cs = ClassSet.find s b.b_classes

      let add_class (b : t) (s : Class.t) : cs =
	let cnum = b.b_ccount in
	b.b_classes <- ClassSet.add s cnum b.b_classes;
    	b.b_ccount  <- succ b.b_ccount;
	cnum
	  
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
	  then raise (Failure "invalid transition : trying to create a transition from state and character class leading to a different state");
	with Not_found -> ignore (add_transition b source cs target)

      let pprint_transitions b = 
	AB.iter (fun i v -> AB.iter (fun j (cs,state) -> Printf.printf "%d : %d -> cs : %d, state : %d\n" i j cs state) v) b.b_transitions

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
	List.fold_left (fun b x -> goto q b x) b (Class.make q)

      let pprint b = 
	print_string "state count   : "; print_int (state_count b); print_newline ();
	print_string "class count   : "; print_int (class_count b); print_newline ();
	print_endline "states: -----------"; 
	pprint_stateset b; 
	print_endline "classes: ----------"; 
	pprint_classset b; 
	print_endline "transitions: ------"; 
	pprint_transitions b;
	print_endline "-------------------"
	
      let make re = 
	let b = {
	  b_ccount       = 0;               
	  b_states       = (* QSet.add re 0 *) QSet.empty;
	  b_accept       = StateSet.empty;
	  b_classes      = ClassSet.empty;  
	  b_transitions  = AB.create 5 (AB.create 5 (-1,-1));
	} in
	ignore (register_state b re);
	let res = explore re b in
(*	pprint b; *)
	res

      (* construct the DFA *)
      let build (b : t) : dfa = 
	let c_count = class_count b in
	let fillup_state v ab =
	  for i = 0 to pred (AB.length ab) do
	    let cs, t = AB.nth ab i in
	    v.(cs) <- t
	  done
	in
	let make_state_transition b i = 
	  let v = Array.make c_count (-1) in
	  fillup_state v (AB.nth b.b_transitions i);
	  v
	in
	let c = Array.make c_count Class.empty in 
	ClassSet.iter (fun cs i -> c.(i) <- cs) b.b_classes;
	let accept = Array.make (state_count b) false in
	StateSet.iter (fun cs -> accept.(cs) <- true) b.b_accept;
	{
	  classes     = c;
	  accept      = accept;
	  transitions = Array.init (state_count b) (make_state_transition b);
	}
	  
    end (* Builder *)

    let make re = 
      Builder.build (Builder.make re)

    let dump dfa =
      Array.iteri (fun k v -> print_int k; print_string ": "; Class.pprint v; print_newline ()) dfa.classes;
      Array.iteri (fun k v -> print_int k; print_string ": "; print_string (string_of_bool v); print_newline ()) dfa.accept;
      Array.iteri (fun i a -> Printf.printf "%02d : " i; Array.iter (fun v -> Printf.printf "%02d " v) a; print_newline ()) dfa.transitions

    let string_match kind dfa s =
      let ccount = Array.length dfa.classes in
      let rec find_class dfa c i len =
	if i >= len
	then -1
	else
	  if Class.contains dfa.classes.(i) c 
	  then i
	  else find_class dfa c (succ i) len
      in
      let result state i = 
	if dfa.accept.(state) 
	then Some i
	else None
      in
      let rec first_match_re dfa s state i m ccount =
	if i >= m
	then result state i
	else 
	  let rec loop_classes dfa s i c = 
	    match find_class dfa (S.get s i) c ccount with
		-1 -> result state i
	      | nc ->
		match dfa.transitions.(state).(nc) with
		  | -1     -> loop_classes dfa s i (succ nc)
		  | nstate ->
		    if dfa.accept.(nstate)
		    then
		      result nstate (succ i)
		    else
		      match first_match_re dfa s nstate (succ i) m ccount with
			| None   -> loop_classes dfa s i (succ nc)
			| r      -> r
	  in loop_classes dfa s i 0

      and match_re fcomp dfa s state i m ccount =
	if i >= m
	then result state i
	else
	  let rec loop_classes dfa s i c =
	    match find_class dfa (S.get s i) c ccount with
		-1 -> result state i
	      | nc -> 
		match dfa.transitions.(state).(nc) with
		  | -1     ->	      
		    loop_classes dfa s i (succ c)
		  | nstate ->
		    let r = 
		      match 
			loop_classes dfa s i (succ nc),
			match_re fcomp dfa s nstate (succ i) m ccount
		      with
			| Some k, Some k' -> Some (fcomp k k')
			| Some k, None       
			| None  , Some k  -> Some k
			| _               -> None
		    in
		    match dfa.accept.(nstate), r with
		      | true,  Some k -> result nstate (fcomp (succ i) k)
		      | true,  None   -> result nstate (succ i)
		      | false, _      -> r
	  in loop_classes dfa s i 0
      in
      let len = (S.length s) in
      match kind with
	  SHORTEST    -> match_re min dfa s 0 0 len ccount
	| LONGEST     -> match_re max dfa s 0 0 len ccount
	| FIRST       -> first_match_re dfa s 0 0 len ccount

  end  (* DFA *)


  (* intermediate string based regexp type *)
  module SRegexp =
  struct

    module Utils =
    struct

      module C = S.Char

    (*
      let rec iterf iterate f s =
      match iterate s with
      Some c -> f c; iterf iterate f s
      | None   -> ()
    *)

      let iter f s =
	for i = 0 to pred (S.length s) do
	  f (S.get s i)
	done
	  
      let riter f s =
	for i = pred (S.length s) downto 0 do
	  f (S.get s i)
	done
	  
    (*
      let init c f =
      let s = S.make c (C.chr 0) in
      for i = 0 to pred c do
      S.set s i (f i)
      done;
      s

      let range a b = 
      let ac = C.code a
      and bc = C.code b in
      let ac, bc = if ac > bc then bc, ac else ac, bc in
      init (bc - ac + 1) (fun i -> C.chr (i + ac))
    *)

      let fold_left f a s =
	let acc = ref a in
	String.iter (fun c -> acc := f !acc c) s;
	!acc

      let fold_right (f : C.t -> 'a -> 'a) (s : S.t) (a : 'a) = 
	let acc = ref a in
	riter (fun c -> acc := f c !acc) s;
	!acc

    end
    
    type s_re = 
      | SAny
      | SAtom    of S.t
      | SSet     of char_t list
      | SRange   of char_t * char_t
      | SConcat  of s_re * s_re
      | SKleene  of s_re
      | SRepeat  of s_re * int * int (* r{n,m} *)
      | SOr      of s_re * s_re
      | SAnd     of s_re * s_re
      | SNot     of s_re
      | Re       of re

    (* convert a string based regexp to a char based one *)
    let rec convert : s_re -> re = function
      | SAny             -> Any
      | SAtom   s        -> (
	match S.length s with
	    0     -> EmptySet
	  | _     -> Utils.fold_right (fun x acc -> concat (Set (CS.singleton x)) acc) s Epsilon
      )
      | SSet    s        -> (
	match s with 
	    []    -> EmptySet
	  | [x]   -> Set (CS.singleton (x))
	  | x::xs -> Set (List.fold_right (fun x acc -> CS.merge (CS.singleton x) acc) s (CS.singleton x)
	  )
      )
      | SRange  (a,b)    -> Set (CS.range a b)
      | SConcat (r1, r2) -> concat (convert r1) (convert r2)
      | SKleene r        -> kleene (convert r)
      | SRepeat (r, x, y) -> 
	assert ((x < y) && (x >= 0));
	let m = y - x 
	and r = convert r in
	if x  = 0 
	then repeat m r
	else
	  let c = if x > 1 
	    then List.fold_right (fun x acc -> concat acc x) (Array.to_list (Array.make (pred x) r)) r 
	    else r
	  in concat c (repeat m r)
      | SOr     (r1, r2) -> or_op (convert r1) (convert r2)
      | SAnd    (r1, r2) -> and_op (convert r1) (convert r2)
      | SNot    r        -> not_op (convert r)
      | Re      r        -> r

  end

  (** regexp parser parameters module *)
  module type PARSER_CONF =
  sig

    type stream

    exception EOF

    type token =  
	LPARENS
      | RPARENS
      | QMARK
      | STAR
      | PLUS
      | MINUS
      | PIPE
      | LBRACE
      | RBRACE
      | LBRACKET
      | RBRACKET
      | BACKSLASH
      | DOT
      | COMMA
      | DOLLAR
      | CARRET
      | ALPHA of char
      | NUM   of char 
      | OTHER of C.t

    (** return the token at position in stream *)
    val token : stream -> int -> token

    (** tells whether we reached the end of the stream *)
    val eof : stream -> int -> bool
      
    (** return the character corresponding to the given token *)
    val to_char : token -> C.t


  end
  (** note : dealing with newline issues is userland responsibility, by crafting 
      the correct regexpr *)

  (** parser module for regular expressions *)
  module Parser(Conf : PARSER_CONF) =
  struct

    (* more pseudo constructors *)
    let plus re = concat re (kleene re)

    let power n re =
      let rec fold acc k =
	if k = 0
	then acc
	else fold (concat re acc) (pred k)
      in
      fold re n

    let powerrep b e re = 
      match b with 
	  0 -> (repeat (e-b) re)
	| _ -> concat (power b re) (repeat (e-b) re)

    open Conf

    exception Failure of stream * int

    type infix = Ipipe

    type postfix = Pqmark | Pplus | Pstar | Prange of int * int option

    type lexeme =
	LPar
      | RPar
      | Infix of infix
      | Postfix of postfix
      | R of re

    let rec dump = 
      let postfix = function
	| Pqmark             -> print_string "Pqmark"
	| Pplus              -> print_string "Pplus"
	| Pstar              -> print_string "Pstar"
	| Prange (b, Some e) -> print_string ("Prange ("^string_of_int b^","^string_of_int e^")")
	| Prange (b, None)   -> print_string ("Prange ("^string_of_int b^")")
      and infix = function
	| Ipipe  -> print_string "Ipipe"
      in
      let lexeme = function
	| LPar      -> print_string "("
	| RPar      -> print_string ")"
	| Infix o   -> infix o
	| Postfix o -> postfix o
	| R re      -> pprint re
      in function
	| x::xs -> dump xs; print_string " "; lexeme x
	| []    -> ()

    let merge  = CS.merge
    let range  = CS.range
    let single = CS.singleton
    let addsingle x y = CS.merge x (CS.singleton y)
	
    let rconcat a b = concat b a

    let parse_int (s : stream) (i : int) : int * int = 
      let rec int0 accum s i =
	if eof s i 
	then i, accum
	else 
	match token s i with
	  | NUM c        -> (
	      let v = (Char.code c) - (Char.code '0') in
	      int0 (accum * 10 + v) s (succ i)
	  )
	  | _            -> i, accum
      in
      int0 0 s i

    let guard_eof s i (f : unit -> 'a) =
      if eof s i 
      then raise (Failure (s,i))
      else f ()
	
    let parse_backslash s i =
      if eof s i 
      then succ i, Set (single (to_char BACKSLASH))  (* fix-me: is this correct ? *)
      else succ i, Set (single (to_char (token s i)))
	
    (* either :
       - nothing read
       - one int read
       - one int and comma read
    *)
    let parse_range s i =
      let rec range0 s i =
	let ni, b = parse_int s i in 
	range1 b s ni
      and range1 (b : int) s i =
	guard_eof s i (fun () ->
	  match token s i with
	      RBRACE   -> succ i, Prange (b, None)
	    | COMMA    -> let ni, k = parse_int s (succ i)
			  in range2 b k s ni
	    | _        -> raise (Failure (s, i))
	)
      and range2 (b : int) (e : int) s i =
	guard_eof s i (fun () ->
	  match token s i with
	      RBRACE   -> succ i, Prange (b,Some e)
	    | _        -> raise (Failure (s, i))
	)
      in 
      range0 s i

      (* either :
	 - zero char read
	 - one char read
	 - one char and minus read
      *)
    let parse_set s i =
      let rec set0 cs s i =
	guard_eof s i (fun () ->
	  match token s i with
	    | RBRACKET  -> succ i, cs
	    | BACKSLASH -> set1 cs (backslash s (succ i)) s (i + 2)
	    | MINUS     -> raise (Failure (s,i))
	    | c         -> set1 cs (Conf.to_char c) s (succ i)
	)
      and set1 cs b s i =
	guard_eof s i (fun () ->
	  match token s i with
	    | RBRACKET  -> succ i, addsingle cs b
	    | BACKSLASH -> set1 (addsingle cs b) (backslash s (succ i)) s (i + 2)
	    | MINUS     -> set2 cs b s (succ i)
	    | c         -> set1 (addsingle cs b) (Conf.to_char c) s (succ i)
	)
      and set2 cs b s i =
	guard_eof s i (fun () ->
	  match token s i with
	    | RBRACKET  -> succ i, addsingle (addsingle cs b) (Conf.to_char MINUS)
	    | BACKSLASH -> set0 (merge cs (range b (backslash s (succ i)))) s (i + 2)
	    | c         -> set0 (merge cs (range b (Conf.to_char c))) s (succ i)
	)
      and backslash s i =
	guard_eof s i (fun () ->
	  Conf.to_char (token s i)
	)
      in 
      guard_eof s i (fun () ->
	match token s i with
	  | RBRACKET  -> succ i, EmptySet
	  | CARRET    -> let i, cs = set0 CS.empty s (succ i) in 
			 i, Not (Set cs)
	  | c         -> let i, cs = set1 CS.empty (Conf.to_char c) s (succ i)in
			 i, Set cs
      )

    let parse (s : stream) : re =
      let rec parse s i stack = 
	match stack with
	  | Postfix o::R x::xs    -> postfix (parse s i) o x xs
	  | R x::Infix o::R y::xs -> infix (parse s i) o x y xs
	  | RPar::R x::LPar::xs   -> shift s i (R x::xs)
	  | RPar::R x::R y::xs    -> parse s i (RPar::R (concat y x)::xs)
	  | _                     -> shift s i stack

      and postfix f (o : postfix) (re : re) stack =
	match o with
	    Pqmark              -> f (R (repeat 1 re)::stack)
	  | Pstar               -> f (R (kleene re)::stack)
	  | Pplus               -> f (R (plus re)::stack)
	  | Prange (b, None)    -> f (R (power b re)::stack)
	  | Prange (b, Some e)  -> f (R (powerrep b e re)::stack)
	  
      and infix f o x y stack =
	match o with
	  | Ipipe -> f (R (or_op x y)::stack)

      and shift s i stack =
	let ni = succ i in
	if eof s i 
	then (
	  dump stack;
	  print_newline ();
	  finish s i stack
	)
	else
	match token s i with
	    LPARENS    -> parse s ni (LPar::stack)
	  | RPARENS    -> parse s ni (RPar::stack)
	  | QMARK      -> parse s ni (Postfix Pqmark::stack)
	  | STAR       -> parse s ni (Postfix Pstar::stack)
	  | PLUS       -> parse s ni (Postfix Pplus::stack)
	  | PIPE       -> parse s ni (Infix Ipipe::stack)
	  | LBRACE     -> let ni, r = parse_range s (succ i) in
			  parse s ni (Postfix r::stack)
	  | LBRACKET   -> let ni, r = parse_set s ni in 
			  parse s ni (R r::stack)
	  | BACKSLASH  -> let ni, r = parse_backslash s ni in
			  parse s ni (R r::stack)
	  | DOT        -> parse s ni (R Any::stack)
	  (*
	    | DOLLAR     ->
	    | CARRET     -> 
	  *)
	  | c          -> parse s ni (R (Set (single (Conf.to_char c)))::stack)

      and finish s i = function
	  | Postfix o::R x::xs    -> postfix (finish s i) o x xs
	  | R x::Infix o::R y::xs -> infix (finish s i) o x y xs
	  | RPar::R x::LPar::xs   -> finish s i (R x::xs)
	  | RPar::R x::R y::xs    -> finish s i (RPar::R (concat y x)::xs)
	  | R x::R y::xs          -> finish s i (R (concat y x)::xs)
	  | [R x]                 -> x
	  | stack                 ->  dump stack; raise (Failure (s,i))

      in parse s 0 []
	    
  end

end
