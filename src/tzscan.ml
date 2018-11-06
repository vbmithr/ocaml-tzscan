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
