(*

  implementation of a Regular Expression DFA constructor from 
  RE derivatives.

  Papers :

  [1] Derivatives of regular expressions (Brzozowski)

  [2] Regular expression derivatives reexamined (Owens,Reppy,Turron)

*)

module type STRING_SIMPLE =
sig
  type t
  type char
  val make     : int -> char -> t
  val length   : t -> int
  val sub      : t -> int -> int -> t
  val get      : t -> int -> char
  val set      : t -> int -> char -> unit
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
  type t
  type char
  val length    : t -> int
  val sub       : t -> int -> int -> t
  val get       : t -> int -> char
  val compare   : t -> t   -> int
  val substract : t -> t   -> t option
end

module Make (S : STRING) =
struct

  type string_t = S.t

  let length    = S.length
  let sub       = S.sub
  let get       = S.get
  let substract = S.substract

  (* regexp language def *)
  type t = 
      EmptySet
    | EmptyString
    | Any
    | Atom     of S.char
    | Concat   of t * t     
    | Kleene   of t           (* '*' operator *)
    | Or       of t * t
    | And      of t * t
    | Not      of t

  (* delta function - v (nu) in [2] *)
  let rec delta r = 
    match r with
	EmptySet      -> EmptySet
      | EmptyString   -> EmptyString
      | Any           -> EmptySet
      | Atom _        -> EmptySet
      | And (a,b)
      | Concat (a,b)  -> (
	match delta a, delta b with
	    EmptySet, _ -> EmptySet
	  | _, EmptySet -> EmptySet
	  | _, _        -> EmptyString
      )
      | Kleene a      -> EmptyString
      | Or (a,b)      -> (
	match delta a, delta b with
	    EmptyString, _ -> EmptyString
	  | _, EmptyString -> EmptyString
	  | _, _           -> EmptySet
      )
      | Not (a)       -> (
	match delta a with
	    EmptySet       -> EmptyString
	  | _              -> EmptySet
      )

  let concat a b = 
    match a, b with
	EmptySet, _
      | _, EmptySet    -> EmptySet 
      | EmptyString, _ -> b
      | _, EmptyString -> a
      | _, _           -> Concat (a,b)
	
  let kleene a = Kleene a

  let and_op a b =
    match a, b with
	EmptySet, _ 
      | _, EmptySet    -> EmptySet
      | _, _           -> And (a,b)

  let or_op a b = 
    match a, b with
	EmptySet, _    -> b
      | _, EmptySet    -> a
      | _, _           -> Or (a,b)

  let not_op a = Not a

  (* compute the regexp derivation - d (delta) in [2] *)
  let rec derive c r = 
    match r with
	EmptySet          -> EmptySet
      | EmptyString       -> EmptySet
      | Any               -> EmptyString
      | Atom a when c = a -> EmptyString
      | Atom _            -> EmptySet
      | Concat (a,b)      ->
	or_op (concat (derive c a) b) (concat (delta a) (derive c b))
      | Kleene a          -> concat (derive c a) (kleene a)
      | And (a,b)         -> and_op (derive c a) (derive c b)
      | Or  (a,b)         -> or_op (derive c a) (derive c b)
      | Not a             -> not_op (derive c a)

  (* check whether s is parsed by re *)
  let match_ re s : bool = 
    let rec match_re re s i m =
      if i >= m then delta re = EmptyString
      else
	match derive (S.get s i) re with
	    EmptySet    -> false
	  | d           -> match_re d s (succ i) m
    in
    match_re re s 0 (S.length s)


  (* input symbol set *)
  module SymbolSet = Set.Make(S)

  let empty_set = SymbolSet.empty

  (* a transition is an arc from a given state to an another state
     this arc carries one or more input symbols
     when using the dfa, the arc will be picked if it carries the proper symbol
     => at buildtime, we need a quick way of finding an arc between 2 states
        = map of state * state to arcs
        and a target state
        = list of states
     => at runtime, we need a quick way of checking which arc carries a given symbol
        = map of symbols to arcs
  *)

(*
  module Transition =
  struct

    type 'a t =  'a * SymbolSet.t
	
  end
    
  (* DFA state definition *)
  module BuildState = 
  struct
    type re_t = t

    type t = {re : re_t ; transitions : (t Transition.t) list }

    let make re = { re; transitions = []}


  end

  module StateSet =
  struct

    type t = BuildState.t list
    
    let find s v = 
      let rec find l v =
	match l with
	    []    -> None
	  | x::xs -> if x = v then Some x else find xs v
      in find s v
      
  end

  type transition_fun_t = BuildState.t -> S.t -> BuildState.t

  (* DFA definition :
     - Set of states
     - starting state
     - Set of final states
     - state transition function
  *)
  type 'a dfa_t = StateSet.t * State.t * StateSet.t * transition_fun_t

  let rec goto s c (qset, delta) =
    let qc = derive c (State.re s)
    in
    (* search the state(qc) *)
        (* found? 
       yes: find an arc from the current state to that state 
            found? yes: add the symbol c to its symbolset 
                   no : create the arc and with symbol c
       no: create the state and the arc to it with symbol c
       continue from state of qc
    *)
    match StateSet.find qset qc with
	Some s' -> Transition.add s s' c (* a state exists, we add/create the arc with the symbol *)
      | None    -> 
      in explore (Q , delta , qc )

  and explore (Q, delta, q) = fold (goto q) (Q, delta) 

(*
  let make r =
    let q0 = derive EmptyString r in
    let (Q, delta) = explore ({q0 }, {}, q0 ) in
    let F = {q | q ∈ Q and ν(q) = ε}
    in Q, q0 , F, delta
*)
*)
end
