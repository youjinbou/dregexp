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

module type ELT =
sig

  type t

  val succ : t -> t
  val pred : t -> t

  val compare : t -> t -> int

  val to_string : t -> string

end

module type S =
sig

  type e
  type t

  val empty : t
  val singleton : e -> t
  val range : e -> e -> t

  val contains : t -> e -> bool
  val compare : t -> t -> int

  val intersect : t -> t -> t
  val merge : t -> t -> t
  val substract : t -> t -> t

  val iter : (e -> unit) -> t -> unit

  val first : t -> e
  val last  : t -> e

  val to_string : t -> string

  val pprint : t -> unit

end

module Make: functor (E : ELT) -> S with type e = E.t and type t = (E.t * E.t) list
