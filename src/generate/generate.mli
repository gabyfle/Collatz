(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*  Copyright (C) 2024                                                       *)
(*    Gabriel Santamaria                                                     *)
(*                                                                           *)
(*                                                                           *)
(*  Licensed under the Apache License, Version 2.0 (the "License");          *)
(*  you may not use this file except in compliance with the License.         *)
(*  You may obtain a copy of the License at                                  *)
(*                                                                           *)
(*    http://www.apache.org/licenses/LICENSE-2.0                             *)
(*                                                                           *)
(*  Unless required by applicable law or agreed to in writing, software      *)
(*  distributed under the License is distributed on an "AS IS" BASIS,        *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(*  See the License for the specific language governing permissions and      *)
(*  limitations under the License.                                           *)
(*                                                                           *)
(*****************************************************************************)

module G = Bigarray.Array1

module type TData = sig
  type t

  val create : int -> t
  val close : t -> unit
  val get : t -> int -> int
  val set : t -> int -> int -> unit
  val mem : t -> int -> bool
end

module Dataset : sig
  include TData

  val set_batch :
    t -> int -> (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t -> unit

  val var_mean : t -> int -> float * float
  val csv : t -> string -> unit
end

module Memory : sig
  include TData

  val raw : t -> (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t
end

val generate : int -> Memory.t * Dataset.t
