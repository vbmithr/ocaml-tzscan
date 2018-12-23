(*
 * Copyright (c) 2018 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module RPC : module type of Resto_cohttp.Client.Make(Resto_encoding)
module Media_type : module type of Resto_cohttp.Media_type.Make(Resto_encoding)
open RPC
module Yojson_encoding :
  module type of Json_encoding.Make(Json_repr.Yojson)

val json : Media_type.t
val all_media_types : Media_type.t list

module V2 : sig
  val date :
    ([ `GET ], unit, unit, unit, unit, float list, unit) Service.t
    (** date on TzScan node. *)
end

module V3 : sig
  val snapshot_levels :
    ([ `GET ], unit, unit, unit, unit, int32 list, unit)
      Service.t

  type endorsement = {
    block : string ;
    level : int64 ;
    endorser : string ;
    slots : int list ;
    op_level : int64 ;
    priority : int ;
    timestamp : string ;
  }

  val endorsement_encoding :
    endorsement Json_encoding.encoding

  type operation_kind =
    | Endorsement of endorsement
    | Unknown of unit

  type operation = {
    hash : string ;
    block_hash : string ;
    network_hash : string ;
    kind : operation_kind ;
  }

  val operation_encoding : operation Json_encoding.encoding

  val operations :
    ([ `GET ], unit, unit * string, int option, unit, operation list, unit)
      Service.t
end
