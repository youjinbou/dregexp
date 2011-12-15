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

open OUnit

module PChar = Char

let code = PChar.code;;
let chr  = PChar.chr;;

Random.self_init ();;

module RangeSetTest =
struct

  open Rangeset

  module Char = 
  struct 

    type t = char
    let succ x = chr (succ (code x))
    let pred x = chr (pred (code x))
    let compare = Pervasives.compare 

    let to_string = PChar.escaped

    let print c = print_int (code c)

  end

  module CR = Make(Char)

  let random_char () =
    chr ((Random.int 253) + 1)

  let check_singleton cs c inverse = 
    let msg b = 
      Printf.sprintf "character %d %sin rangeset %s" (code c) b (CR.to_string cs)
    in
    if inverse 
    then
      assert_bool (msg "")  (not (CR.contains cs c))
    else
      assert_bool (msg "not ") (CR.contains cs c)
	
  let check_range cs b e inverse =
    if inverse 
    then
      for i = (code b) to (code e) do
	assert_bool (Printf.sprintf "character %d in rangeset" i) (not (CR.contains cs (chr i)))
      done
    else
      for i = (code b) to (code e) do
	assert_bool (Printf.sprintf "character %d not in rangeset" i) (CR.contains cs (chr i))
      done

  let print name x = 
    print_string (name^" : ");
    CR.pprint x;
    print_newline ()

  let check_intersect cs b1 e1 b2 e2 =
    for i = 1 to 254 do
      check_singleton cs (chr i) 
	 (not ((i >= code b1) && (i <= code e1) &&
	   (i >= code b2) && (i <= code e2)))
    done

  let check_merge cs b1 e1 b2 e2 =
    for i = 1 to 254 do
      check_singleton cs (chr i) 
	((i < (code b1) || (i > code e1)) 
	 && ((i < code b2) || (i > code e2)))
    done

  let check_substract cs b1 e1 b2 e2 =
    for i = 1 to 254 do
      check_singleton cs (chr i)
	((i < code b1) || (i > code e1)
	 || ((i >= code b2) && (i <= code e2)))
    done

  let test_f f comb =
    let b1 = random_char ()
    and b2 = random_char ()
    and e1 = random_char ()
    and e2 = random_char () in
    let b1,e1 = if b1 <= e1 then b1, e1 else e1,b1
    and b2,e2 = if b2 <= e2 then b2, e2 else e2,b2 in
    let cs1 = CR.range b1 e1
    and cs2 = CR.range b2 e2 in
    f (comb cs1 cs2) b1 e1 b2 e2

  let test_singleton_f f comb =
    let x  = random_char ()
    and b1 = random_char ()
    and e1 = random_char () in
    let b1, e1 = if b1 <= e1 then b1, e1 else e1, b1 in
    let cs1 = CR.range b1 e1
    and cs2 = CR.singleton x in
    f (comb cs1 cs2) b1 e1 x x

  let test_intersect () =
    test_singleton_f check_intersect CR.intersect;
    test_f check_intersect CR.intersect

  let test_merge () =
    test_singleton_f check_merge CR.merge;
    test_f check_merge CR.merge

  let test_substract () =
    test_singleton_f check_substract CR.substract;
    test_f check_substract CR.substract

  let test_empty () =
    let b1 = random_char ()
    and e1 = random_char () in
    let cs1 = CR.range b1 e1 
    and cse = CR.empty in
    assert_bool "empty contains a value" (not (CR.contains cse b1));
    assert_bool "intersect empty _ is not empty" (0 = CR.compare cse (CR.intersect cs1 cse));
    assert_bool "substract _ empty is not _" (0 = CR.compare cs1 (CR.substract cs1 cse));
    assert_bool "merge _ empty is not _" (0 = CR.compare cs1 (CR.merge cs1 cse))

  let test_list = TestLabel (
    "[ Rangeset ]",
    TestList [
      TestLabel ("empty", TestCase test_empty);
      TestLabel ("intersect", TestCase test_intersect);
      TestLabel ("merge", TestCase test_merge);
      TestLabel ("substract", TestCase test_substract);
    ]
  )

end

module Char : Regexp.CHAR with type t = char = 
struct 
  type t = char
  let succ x = Char.chr (succ (Char.code x))
  let pred x = Char.chr (pred (Char.code x))
  let compare = Pervasives.compare 

  let to_string = Char.escaped

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

  (* lowercase alphabet *)
  let az =
    convert (SKleene(SRange('a','z')))

  let abac = 
    convert (SOr (SOr (SAtom "a",SAtom "ba"), SAtom "c"))

  let alpha = SOr(SRange ('a','z'),SRange ('A', 'Z'))
  let num   = SRange('0','9')

  (* simple web url : 
     w = [A-Za-z0-9-_]+
     http://\w(.\w)*(/[A-Za-z0-9-_.]+)*(/?)([?](\w=\w)* )? 
    *)
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
      and params =
	let param = 
	  SConcat (
	    word,
	    SConcat (SAtom "=", word)
	  ) in
	SConcat (SAtom "?", SKleene (param))

      in
      SConcat(SConcat(address,page),SRepeat(params,0,1))
    )

  (* 
     simplified form of email address :
     email = <name>([-.+_]<name>)*@<domain>
  *)
  let email = 
    convert (
      let alphanum = SOr(alpha, num) in
      let dletter = SOr(SSet ['-';'_' ], alphanum) in
      let name = SConcat(alpha,SKleene(alphanum))
      and punct = SSet [ '-' ; '.' ; '+'; '_' ]
      and arob  = SSet [ '@' ] 
      and dword = SConcat (dletter, SKleene(dletter)) in
      let domain = 
	SConcat ( SKleene (SConcat (dword, SSet ['.'])),
		  SConcat ( SConcat (dword, SSet ['.']),
			    dword
		  )
	)
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

  let abcdOrbcde =
    let abcd = SAtom "abcd"
    and bcde = SAtom "bcde"
    in
    convert (SOr (abcd,bcde))

  let a_b =
    let a = SSet [ 'a' ]
    and b = SSet [ 'b' ]
    in
    convert (SConcat (a, SConcat (SAny, b)))

  type test_data = {
    name : string;
    re   : RE.re;
    good : string list;
    bad  : string list;
  }

  let data =
    [ { 
      name = "url";
      re   = url;
      good = [ "http://www.yahoo.fr" ; "http://www.yahoo.fr/" ; "http://www.yahoo.fr/mail/admin" ; "http://www.yahoo.fr/mail/admin/"; "http://www.yahoo.fr/mail?foo=bar" ];
      bad = [ "http://www.yahoo.fr/mail?foo" ];
      };{ 
      name = "email";
      re   = email;
      good = [ "youjinbou.foobar@github.com" ];
      bad  = [ "gggqsdfq@@stuff.com" ; "xxx+yyy@strength..com" ];
      };{ 
      name = "ab{0,5}a";
      re   = ab5a;
      good = [ "aa"; "aba"; "abba"; "abbba"; "abbbba"; "abbbbba" ];
      bad  = [ "abbbbbba" ; "abbbabbbba" ];
      };{ 
      name = "ab{0,3}c{0,2}d";
      re   = ab3c2d;
      good = [ "ad" ; "abd"; "acd"; "abcd" ];
      bad  = [ "aa" ; "acbd" ; "abbbbcd" ; "abcccd" ];
      };{ 
      name = "abcd|bcde";
      re   = abcdOrbcde;
      good = [ "abcd" ; "bcde" ];
      bad  = [ "abce" ; "abde" ; "acde" ];
      };{ 
      name = "a.b";
      re   = a_b;
      good = [ "aab" ; "abb" ; "acb" ; "a.b" ];
      bad  = [ "bab" ; "abd" ; "a_bc" ];
      }
    ]

  let assert_list f l =
    List.iter (fun x -> assert_bool ("failed for "^x) (f x)) l

  let assert_raw r v =
    match RE.string_match RE.LONGEST r v with
	Some(r,l) when l = String.length v -> true
      | _                                  -> false
      
  let assert_dfa r v =
    match RE.DFA.string_match RE.LONGEST r v with
	Some(l) when l = String.length v -> true
      | _                                -> false

  let check assertf r gl bl =
    assert_list (fun x -> assertf r x) gl;
    assert_list (fun x -> not (assertf r x)) bl 

  let test_raw () = 
    let check_re x = check assert_raw x.re x.good x.bad in
    List.iter check_re data

  let test_dfa () =
  let check_dfa x = 
    let dfa = RE.DFA.make x.re in
    check assert_dfa dfa x.good x.bad
  in
  List.iter check_dfa data
    
  let test_list = TestLabel ( 
    "[ match ]",
    TestList [
      TestLabel ("raw",TestCase test_raw); 
      TestLabel ("dfa",TestCase test_dfa);
    ]
  )

  module ParserTest =
  struct

    module Conf =
    struct
      
      type stream = string

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
	| NUM of char
	| OTHER of char

      let token s i =
	if i < String.length s 
	then 
	  match s.[i] with 
	    | '('  -> LPARENS
	    | ')'  -> RPARENS
	    | '?'  -> QMARK
	    | '*'  -> STAR
	    | '+'  -> PLUS
	    | '-'  -> MINUS
	    | '|'  -> PIPE
	    | '{'  -> LBRACE
	    | '}'  -> RBRACE
	    | '['  -> LBRACKET
	    | ']'  -> RBRACKET
	    | '\\' -> BACKSLASH
	    | '.'  -> DOT
	    | ','  -> COMMA
	    | '$'  -> DOLLAR
	    | '^'  -> CARRET
	    | c when c >= '0' && c <= '9' -> NUM c
	    | c when (c >= 'A' && c <= 'Z')
	    || (c >= 'a' && c <= 'z') -> ALPHA c
	    | c    -> OTHER c
	else
	  raise EOF
	    
      let eof s i =
	i >= String.length s

      let to_char = function
	| LPARENS      -> '('
	| RPARENS      -> ')'
	| QMARK        -> '?'
	| STAR         -> '*'
	| PLUS         -> '+'
	| MINUS        -> '-'
	| PIPE         -> '|'
	| LBRACE       -> '{'
	| RBRACE       -> '}'
	| LBRACKET     -> '['
	| RBRACKET     -> ']'
	| BACKSLASH    -> '\\'
	| DOT          -> '.'
	| COMMA        -> ','
	| DOLLAR       -> '$'
	| CARRET       -> '^'
	| ALPHA c
	| NUM c
	| OTHER c      ->  c

    end

    module Parser = RE.Parser(Conf)

    open Parser

    let url_s = "http://[-A-Za-z0-9_]+(\\.[-A-Za-z0-9_]+)*(/[-A-Za-z0-9._]+)*(/?)([?]([-A-Za-z0-9_]+=[-A-Za-z0-9_]+)*)?"

    let email_s = 
      let alpha    = "[A-Za-z]"
      and alphanum = "[A-Za-z0-9]"
      and dletter  = "[-_A-Za-z0-9]" in
      let name  = alpha^alphanum^"*"
      and dword = dletter^"+" in
      let domain = "("^dword^"\\.)*"^dword^"\\."^dword
      in
      name^"([-.+_]"^name^")*@"^domain

    let test_url () =
      assert_bool "parser failed on url" (0 = RE.compare url (parse url_s))

    let test_email () =
      assert_bool "parser failed on email" (0 = RE.compare email (parse email_s))

    let test_list = TestLabel (
      "[ Parser ]",
      TestList [
	TestLabel ("url", TestCase test_url);
	TestLabel ("email", TestCase test_email);
      ]
    )
    
  end

end

let test_suite = 
  TestLabel ("[ Test Suite ]",
	     TestList [
	       RangeSetTest.test_list; 
	       RegexpTest.test_list;
	       RegexpTest.ParserTest.test_list; 
	     ]
  )

let _ = 
  run_test_tt test_suite

