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

module type STRING =
sig
  module Char : CHAR
  type t
  val length : t -> int
  val get : t -> int -> Char.t
end

module Make :
  functor (S : STRING) ->
sig

  module C :
  sig 
    
    type t = S.Char.t

  end 

  type string_t = S.t
  type char_t = S.Char.t

  module CS : Rangeset.S with type e = C.t and type t = (C.t * C.t) list

  type t
  type re = t


  type match_kind = SHORTEST | LONGEST | FIRST
  type match_solution = re * int

  val compare : t -> t -> int
  val pprint : t -> unit
  val delta : t -> t
  val nullable : t -> bool
  val intersect : CS.t -> CS.t -> t
  val concat : t -> t -> t
  val kleene : t -> t
  val repeat : int -> t -> t
  val and_op : t -> t -> t
  val or_op : t -> t -> t
  val not_op : t -> t
  val derive : CS.e -> t -> t
  val string_match : match_kind -> t -> S.t -> match_solution option


  module DFA :
  sig
    
    type t

    module Builder :
    sig

      type dfa = t
      type t

      exception Failure of String.t

      val pprint : t -> unit
      val make : re -> t
      val build : t -> dfa

    end

    val make : re -> t
    val dump : t -> unit

    val string_match : match_kind -> t -> S.t -> int option

  end

  module SRegexp :
  sig

    type s_re =
        SAny
      | SAtom of S.t
      | SSet of char_t list
      | SRange of char_t * char_t
      | SConcat of s_re * s_re
      | SKleene of s_re
      | SRepeat of s_re * int * int
      | SOr of s_re * s_re
      | SAnd of s_re * s_re
      | SNot of s_re
      | Re of re

    val convert : s_re -> re

  end

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
      | NUM of char
      | OTHER of C.t

    val token : stream -> int -> token
    val eof : stream -> int -> bool
    val to_char : token -> C.t

  end

  module Parser :
    functor (Conf : PARSER_CONF) ->
  sig

    val parse : Conf.stream -> re

  end
end
