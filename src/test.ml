
module RangeSetTest =
struct

  open Rangeset

  module Char = 
  struct 

    type t = char
    let succ x = Char.chr (succ (Char.code x))
    let pred x = Char.chr (pred (Char.code x))
    let compare = Pervasives.compare 

    let print = print_char

  end

  module CR = Make(Char)

  let v1 = CR.range 'a' 'c'  (* [a-c] *)
 
  let v2 = CR.range 'e' 'h'  (* [e-h] *)

  let v3 = CR.range 'i' 'g'  (* [g-i] *)

  let v4 = CR.range 'm' 'q'  (* [m-q] *)

  let v5 = CR.range 'w' 'y'  (* [w-y] *)

  let v6 = CR.intersect v1 v2   (* [] *)

  let v7 = CR.merge (CR.merge v1 v2) (CR.merge v5 v6) (* [a-ce-hw-y] *)

  let v8 = CR.merge (CR.merge v3 v5) v1 (* [a-cg-iw-y] *)

  let v9 = CR.intersect v7 v8 (* [a-cg-hw-y] *)

  let v10 = CR.merge v2 v3  (* [e-i] *)

  let s1 = CR.singleton 'f' (* [f] *)
  let s2 = CR.singleton 't' (* [t] *)
  let s3 = CR.singleton 'o' (* [o] *)
  let s4 = CR.singleton  'a' 
  let s5 = CR.singleton  'c' 


  let r1 = CR.merge v4 s1   (* [fm-q] *)
  let r2 = CR.merge v2 s2   (* [e-ht] *)
  let r3 = CR.merge v1 s3   (* [a-co] *)

  let r4 = CR.substract v2 s1 (* [eg-h] *)
  let r5 = CR.substract v7 s1 (* [a-ceg-hw-y] *)

  let r6 = CR.substract (CR.merge (CR.merge v1 v2) (CR.merge v3 v4)) r4 (* [a-cfim-q] *)
  let r7 = CR.substract v1 v2 (* v1 *)
  let r8 = CR.substract v2 v1 (* v2 *)
  let r9 = CR.substract s1 s1 (* [] *)
  let r10 = CR.substract s2 s1 (* s2 *)
  let r11 = CR.merge s4 s5 (* [ac] *)

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
      "v9 = v7 & v8 = v1 + v5 + ((v2 + v6) & v3)", v9;
      "v10 = v2 + v3", v10;
      "s1", s1;
      "s2", s2;
      "s3", s3;
      "s4", s4;
      "s5", s5;
      "r1 = v4 + s1", r1;
      "r2 = v2 + s2", r2;
      "r3 = v1 + s3", r3;
      "r4 = v2 - s1", r4;
      "r5 = v7 - s1", r5;
      "r6 = v1 + v2 + v3 + v4 - r4", r6;
      "r7 = v1 - v2", r7;
      "r8 = v2 - v1", r8;
      "r9 = s1 - s1", r9;
      "r10 = s2 - s1", r10;
      "r11 = s4 + s5", r11;
    ]

end

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
module MyString : Regexp.STRING with module Char = Char and type t = string =
struct
  module Char = Char

  include String
  type char = Char.t
end

module RegexpTest = 
struct

  module RE = Regexp.Make(MyString)

  open RE.SRegexp


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

  (* simple web url : http://[A-Za-z0-9-_]+(.[A-Za-z0-9-_]+)+(/[A-Za-z0-9-_.]+)*(/?) *)
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
	SConcat (
	  SKleene (SConcat (SAtom "/", folder)),
	  SRepeat(SAtom "/",0,1)
	)
      in
      SConcat(address,page)
    )

  (* 
     simplified form of email address :
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

  let ab5a =
    let a = SSet [ 'a' ]
    and b = SSet [ 'b' ]
    in
    convert (SConcat (a,SConcat (SRepeat (b, 0, 5),a)))

  let ab3c2d =
    let a = SSet [ 'a' ]
    and b = SRepeat (SSet [ 'b' ], 0, 3)
    and c = SRepeat (SSet [ 'c' ], 0, 2)
    and d = SSet [ 'd' ]
    in
    convert (SConcat (a,SConcat (b, SConcat (c, d))))

  let _ =
    let assert_list f l =
      List.iter (fun x -> assert (f x)) l
    in
    let check r gl bl =
      assert_list (RE.string_match r) gl;
      assert_list (fun x -> not (RE.string_match r x)) bl 
    in
    let good_url   = [ "http://www.yahoo.fr" ; "http://www.yahoo.fr/" ; "http://www.yahoo.fr/mail/admin" ; "http://www.yahoo.fr/mail/admin/" ]
    and bad_url    = [ "http://www.yahoo.fr/mail?foo" ]
    and good_email = [ "youjinbou+foobar@github.com" ] (* slow *)
    and bad_email  = [] 
    and good_ab5a  = [ "aa"; "aba"; "abba"; "abbba"; "abbbba"; "abbbbba" ]
    and bad_ab5a   = [ "abbbbbba" ; "abbbabbbba" ]
    and good_ab3c2d = [ "ad" ; "abd"; "acd"; "abcd" ]
    and bad_ab3c2d  = [ "aa" ; "acbd" ; "abbbbcd" ; "abcccd" ]
    in
    check url good_url bad_url;
    check email good_email bad_email;
    check ab5a good_ab5a bad_ab5a;
    check ab3c2d good_ab3c2d bad_ab3c2d

  let _ =
    let acOrbc = convert (SOr (SAtom "ab",SAtom "ac")) in
    RE.DFA.dump (RE.DFA.make acOrbc);
    RE.DFA.dump (RE.DFA.make url);
    RE.DFA.dump (RE.DFA.make email)

end
  
