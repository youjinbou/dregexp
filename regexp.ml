module type STRING_SIMPLE =
sig
  type t
  type char_t
  val length   : t -> int
  val sub      : t -> int -> int -> t
  val get      : t -> int -> char_t
  val compare  : t -> t   -> int
end

(* provides a substract function *)
module Extend (S : STRING_SIMPLE) =
struct

  let length = S.length
  let sub    = S.sub
  let get    = S.get
    
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
  type char_t
  val length    : t -> int
  val sub       : t -> int -> int -> t
  val get       : t -> int -> char_t
  val compare   : t -> t   -> int
  val substract : t -> t   -> t
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
    | Atom     of S.char_t
    | Concat   of t * t
    | Kleene   of t 
    | Or       of t * t
    | And      of t * t
    | Not      of t

  (* delta function *)
  let rec delta r = 
    match r with
	EmptySet      -> EmptySet
      | EmptyString   -> EmptyString
      | String _      -> EmptySet
      | And (a,b)
      | Concat (a,b)  -> (
	match delta a, delta b with
	    EmptySet, _ -> EmptySet
	  | _, EmptySet -> EmptySet
	  | _, _        -> EmptyString
      )
      | Kleene a      -> delta a
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

  (* compute the regexp derivation *)
  let rec deriv c r = 
    match r with
	EmptySet          -> EmptySet
      | EmptyString       -> EmptySet
      | Atom a when c = a -> EmptyString
      | Atom _            -> EmptySet
(*
      | String s          -> String (substract s c)
*)
      | Concat (a,b)      -> (
	let da = deriv c a in
	match delta a with
	    EmptyString -> Concat (da, deriv c b)
	  | _           -> Concat (da, b)
      )
      | Kleene a          -> Concat (deriv c a, Kleene a)
      | And (a,b)         -> And (deriv c a, deriv c b)
      | Or  (a,b)         -> Or (deriv c a, deriv c b)
      | Nor a             -> Not (deriv c a)


  (* input symbol set *)
  module SymbolSet = Set.Make(S)

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
    let qc = deriv c (State.re s)
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

  and explore (Q, delta, q) = fold (goto q) (Q, delta) Σ

(*
  let make r =
    let q0 = deriv EmptyString r in
    let (Q, delta) = explore ({q0 }, {}, q0 ) in
    let F = {q | q ∈ Q and ν(q) = ε}
    in Q, q0 , F, delta
*)

end
