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

(* ---------------------------------------------------------------------

   This code provided under the LGPL License V2


   Buffer code lifted from the original Buffer module distributed with 
   Ocaml:
   "
   Xavier Leroy, projet Cristal, INRIA Rocquencourt         

   Copyright 1996 Institut National de Recherche en Informatique et   
   en Automatique.  All rights reserved.  This file is distributed    
   under the terms of the GNU Library General Public License, with    
   the special exception on linking described in file ../LICENSE."


   Thanks!

   --------------------------------------------------------------------- *)

module type VECTOR =
sig
  type 'a t
  val set : 'a t -> int -> 'a -> unit
  val get : 'a t -> int -> 'a
  val make : int -> 'a -> 'a t
  val sub : 'a t -> int -> int -> 'a t
  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  val length : 'a t -> int
  val max_length : int
end

module Array : VECTOR with type 'a t = 'a array

module type MONO_VECTOR =
sig
  type t
  type e
  val set : t -> int -> e -> unit
  val get : t -> int -> e
  val make : int -> e -> t
  val sub : t -> int -> int -> t
  val blit : t -> int -> t -> int -> int -> unit
  val length : t -> int
  val max_length : int
end

module String : MONO_VECTOR with type t = string and type e = char

module type S =
sig
  type 'a t
  type 'a vector
  val create : int -> 'a -> 'a t
  val contents : 'a t -> 'a vector
  val sub : 'a t -> int -> int -> 'a vector
  val blit : 'a t -> int -> 'a vector -> int -> int -> unit
  val nth : 'a t -> int -> 'a
  val length : 'a t -> int
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val resize : 'a t -> int -> unit
  val add : 'a t -> 'a -> int
  val add_vector : 'a t -> 'a vector -> int
  val add_subv : 'a t -> 'a vector -> int -> int -> int
  val add_buffer : 'a t -> 'a t -> int
  val iter : (int -> 'a -> unit) -> 'a t -> unit
end

module type SMONO =
sig
  type t
  type vector
  type e
  val create : int -> e -> t
  val contents : t -> vector
  val sub : t -> int -> int -> vector
  val blit : t -> int -> vector -> int -> int -> unit
  val nth : t -> int -> e
  val length : t -> int
  val clear : t -> unit
  val reset : t -> unit
  val resize : t -> int -> unit
  val add : t -> e -> int
  val add_vector : t -> vector -> int
  val add_subv : t -> vector -> int -> int -> int
  val add_buffer : t -> t -> int
  val iter : (int -> e -> unit) -> t -> unit
end


module Make : functor (V : VECTOR) -> S with type 'a vector = 'a V.t

module ArrayBuffer : S with type 'a vector = 'a array

module MakeMono : functor (V : MONO_VECTOR) -> SMONO with type vector = V.t and type e = V.e

module StringBuffer : SMONO with type vector = string and type e = char
