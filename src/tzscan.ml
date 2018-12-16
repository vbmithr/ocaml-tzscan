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

module RPC = Resto_cohttp.Client.Make(Resto_encoding)
module Media_type = Resto_cohttp.Media_type.Make(Resto_encoding)
open Resto
open RPC
module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

let json  = {
  Media_type.name = Cohttp.Accept.MediaType ("application", "json") ;
  q = Some 1000 ;
  pp = begin fun _enc ppf raw ->
    try
      Json_repr.pp (module Json_repr.Yojson) ppf
        (Yojson.Safe.from_string raw) with
    | exn ->
        Format.fprintf ppf
          "@[Invalid JSON:@ \
          \ - @[<v 2>Error:@ %s@]\
          \ - @[<v 2>Raw data:@ %s@]@]"
          (Printexc.to_string exn) raw
  end ;
  construct = begin fun enc v ->
    Yojson.Safe.to_string @@
    Yojson_encoding.construct enc v
  end ;
  destruct = begin fun enc body ->
    try
      let json = Yojson.Safe.from_string body in
      try Ok (Yojson_encoding.destruct enc json)
      with Json_encoding.Cannot_destruct (_, exn) ->
        Error (Format.asprintf "%a"
                 (fun fmt -> Json_encoding.print_error fmt)
                 exn)
    with exn ->
      Error (Printexc.to_string exn)
  end
}

let all_media_types = [ json ]

module V2 = struct
  let date =
    Service.get_service
      ~description: "date"
      ~query: Query.empty
      ~output: Json_encoding.(list float)
      ~error: Json_encoding.unit
      Path.(root / "v2" / "date")
end

module V3 = struct
  type endorsement = {
    block : string ;
    level : int64 ;
    endorser : string ;
    slots : int list ;
    op_level : int64 ;
    priority : int ;
    timestamp : string ;
  }

  let tz_encoding =
    let open Json_encoding in
    conv
      (fun s -> (), s) (fun ((), s) -> s)
      (merge_objs unit (obj1 (req "tz" string)))

  let endorsement_encoding =
    let open Json_encoding in
    conv
      (fun { block ; level ; endorser ; slots ; op_level ; priority ; timestamp } ->
         ((), block, level, endorser, slots, op_level, priority, timestamp))
      (fun ((), block, level, endorser, slots, op_level, priority, timestamp) ->
         { block ; level ; endorser ; slots ; op_level ; priority ; timestamp })
      (obj8
         (req "kind" (constant "endorsement"))
         (req "block" string)
         (req "level" int53)
         (req "endorser" tz_encoding)
         (req "slots" (list int))
         (req "op_level" int53)
         (req "priority" int)
         (dft "timestamp" string ""))

  type operation_kind =
    | Endorsement of endorsement
    | Unknown of unit

  let operation_kind_encoding =
    let open Json_encoding in
    union [
      case endorsement_encoding
        (function Endorsement e -> Some e | _ -> None)
        (fun e -> Endorsement e) ;
    ]

  type operation = {
    hash : string ;
    block_hash : string ;
    network_hash : string ;
    kind : operation_kind ;
  }

  let operation_encoding =
    let open Json_encoding in
    conv
      (fun { hash ; block_hash ; network_hash ; kind } ->
         (hash, block_hash, network_hash, kind))
      (fun (hash, block_hash, network_hash, kind) ->
         { hash ; block_hash ; network_hash ; kind })
      (obj4
         (req "hash" string)
         (req "block_hash" string)
         (req "network_hash" string)
         (req "type" operation_kind_encoding))

  let operations =
    Service.get_service
      ~description: "operations"
      ~query: Query.(query (fun n -> n)
                     |+ (opt_field "number" Resto.Arg.int) (fun n -> n)
                     |> seal)
      ~output: Json_encoding.(list operation_encoding)
      ~error: Json_encoding.unit
      Path.(root / "v3" / "operations" /: Resto.Arg.string)
end
