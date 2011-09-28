
module Char : Regexp.CHAR with type t = char = 
struct 
  type t = char
  let succ x = Char.chr (succ (Char.code x))
  let pred x = Char.chr (pred (Char.code x))
  let compare = Pervasives.compare 
  let print = print_char

  let any = 'a'

end


(* expose the types to simplify things a bit *)
module String_simple : Regexp.STRING_SIMPLE with type t = string and module Char = Char =
struct

  module Char = Char

  include String

  type char = Char.t

end

module Utils(S : Regexp.STRING_SIMPLE) =
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

module REString = Regexp.Extend(String_simple) 

module RE = Regexp.Make(REString)

module REU = Utils(REString)

type s_re = 
  | SEmpty
  | SAny
  | SAtom    of string
  | SSet     of char list
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
  | SEmpty           -> RE.Epsilon
  | SAny             -> RE.Any
  | SAtom   s        -> REU.fold_right (fun x acc -> concat (RE.Set (RE.CS.singleton x)) acc) s RE.Epsilon
  | SSet    s        -> List.fold_right (fun x acc -> or_op (RE.Set (RE.CS.singleton x)) acc) s RE.Epsilon
  | SRange  (a,b)    -> RE.Set (RE.CS.range a b)
  | SConcat (r1, r2) -> concat (convert r1) (convert r2)
  | SKleene r        -> kleene (convert r)
  | SOr     (r1, r2) -> or_op (convert r1) (convert r2)
  | SAnd    (r1, r2) -> and_op (convert r1) (convert r2)
  | SNot    r        -> not_op (convert r)
  | Re      r        -> r

(* universal pattern *)
let uni = RE.Kleene(RE.Any)  

(* lowercase alphabet *)
let az =
  convert (SKleene(SRange('a','z')))

let az' = 
  RE.concat az (RE.Set (RE.CS.singleton '!'))


let abac = 
  convert (SOr (SOr (SAtom "a",SAtom "ba"), SAtom "c"))


let alpha = SOr(SRange ('a','z'),SRange ('A', 'Z'))
let num   = SRange('0','9')

(* simple web url : http://[A-Za-z0-9-_]+(.[A-Za-z0-9-_]+)+/[A-Za-z0-9-_.]+(/[A-Za-z0-9-_.]+)* *)
let url = 
  convert (

    let letter = 
      SOr(SOr(alpha, num),
	  SSet [ '-'; '_']
      ) in
    let word = 
      SConcat (letter, SKleene letter) in

    let address = 
      SConcat(
	SAtom "http://",
	SConcat (word, 
		 SKleene(SConcat(SAtom ".",
				 word)
		 )
	)
      )
    and page = 
      let folder =
	let fletter = 
	  SOr(letter,SAtom ".")
	in
	SConcat (fletter, SKleene fletter)
      in
      SOr (SEmpty, 
	   SConcat (
	     SAtom "/",
	     SConcat(folder, SKleene (SConcat (SAtom "/", folder)))
	   )
      )
    in
    SConcat(address,page)
  )

(* 
   some form of email address :
   email = <name>([.+-_]<name>)*@<domain>
*)
let email = 
  convert (
    let alphanum = SOr(alpha, num) in
    let dletter = SOr(alphanum, SSet ['-';'_' ]) in
    let name = SConcat(alpha,SKleene(alphanum))
    and punct = SSet [ '.' ; '+'; '-' ; '_' ]
    and arob  = SSet [ '@' ] 
    and dword = SConcat (dletter, SKleene(dletter)) in
    let domain = SConcat ( SKleene (SConcat (dword, SSet ['.'])),
			   SConcat ( SConcat (dword, SSet ['.']),
				     dword
			   ))
    in
    SConcat (
      SConcat (
	SConcat(name, SKleene (SConcat (punct, name))),
	arob),
      domain
    )
  )

let _ =
  assert (RE.string_match url "http://www.yahoo.fr/mail/admin");
  assert (RE.string_match url "http://www.yahoo.fr/mail/admin/");
  assert (not (RE.string_match url "http://www.yahoo.fr/mail?foo"))

