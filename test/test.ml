open Lwt.Infix
open Tzscan
open Alcotest
open Alcotest_lwt

let base = Uri.of_string "https://api1.tzscan.io"

let basic = [
  test_case "date" `Quick begin fun _ () ->
    RPC.call_service
      ~base all_media_types V2.date () () () >>= fun (_, _, res) ->
    match res with
    | `Ok (Some _) -> Lwt.return_unit
    | _ -> Lwt.fail_with ""
  end ;
]

let () =
  run "tzscan" [
    "basic", basic ;
  ]
