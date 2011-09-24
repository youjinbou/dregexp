
(* expose the types to simplify things a bit *)
module String_simple : Regexp.STRING_SIMPLE with type char = Char.t and type t = string =
struct

  include String

  type char = Char.t

end

module type CHAR = 
sig

  type t
    
  val code : t -> int
  val chr  : int -> t

end

module Utils(S : Regexp.STRING_SIMPLE) (C : CHAR with type t = S.char) =
struct

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

  let fold_left f a s =
    let acc = ref a in
    String.iter (fun c -> acc := f !acc c) s;
    !acc

  let fold_right (f : C.t -> 'a -> 'a) (s : S.t) (a : 'a) = 
    let acc = ref a in
    riter (fun c -> acc := f c !acc) s;
    !acc

end

module REString = Regexp.Extend(String_simple)

module RE = Regexp.Make(REString)

module REU = Utils(REString)(Char)

type s_re = 
  | SEmpty
  | SAny
  | SAtom    of string
  | SRange   of char * char
  | SConcat  of s_re * s_re
  | SKleene  of s_re
  | SOr      of s_re * s_re
  | SAnd     of s_re * s_re
  | SNot     of s_re
  | Re       of RE.t

let concat = RE.concat
and kleene = RE.kleene
and or_op  = RE.or_op 
and and_op = RE.and_op
and not_op = RE.not_op

(* convert a string based regexp to a char based one *)
let rec convert : s_re -> RE.t = function
  | SEmpty           -> RE.EmptyString
  | SAny             -> RE.Any
  | SAtom   s        -> REU.fold_right (fun x acc -> concat (RE.Atom x) acc) s RE.EmptyString
  | SRange  (a,b)    -> REU.fold_right (fun x acc -> or_op (RE.Atom x) acc) (REU.range a b) RE.EmptySet
  | SConcat (r1, r2) -> concat (convert r1) (convert r2)
  | SKleene r        -> kleene (convert r)
  | SOr     (r1, r2) -> or_op (convert r1) (convert r2)
  | SAnd    (r1, r2) -> and_op (convert r1) (convert r2)
  | SNot    r        -> not_op (convert r)
  | Re      r        -> r

(* universal pattern *)
let re_uni = RE.Kleene(RE.Any)  

(* lowercase alphabet *)
let re_az =
  convert (SKleene(SRange('a','z')))

let re_az' = 
  RE.concat re_az (RE.Atom '!')

(* simple web url : http://[A-Za-z0-9-_]+(.[A-Za-z0-9-_]+)+/[A-Za-z0-9-_.]+(/[A-Za-z0-9-_.]+)+ *)
let re_url = 
  let x = 
    let letter = 
      SOr(SOr(SRange ('a','z'),
	      SRange('0','9')),
	  SOr(SRange ('A', 'Z'),
	      SOr (SAtom "-",SAtom "_"))
      ) in
    let word = SConcat (letter, SKleene letter) in
    let address = 
      SConcat(
	SConcat(
	  SAtom "http://",
	  SConcat (word, 
		   SKleene(SConcat(SAtom ".",
				   word)
		   )
	  )
	),
	SOr (SEmpty, SAtom "/")
      )
    and page = 
      let folder =
	let fletter = 
	  SOr(letter,SAtom ".")
	in
	SConcat (fletter, SKleene fletter)
      in
      SConcat(folder, SKleene (SConcat (SAtom "/", folder)))
    in
    SConcat(address,SKleene(page))
  in 
  convert x

