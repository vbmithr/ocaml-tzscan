open Lwt.Infix
open Tzscan
open Alcotest
open Alcotest_lwt

let base = Uri.of_string "https://api6.tzscan.io"

let pp_print_spaced_list =
  Format.pp_print_list ~pp_sep:Format.pp_print_space

let basic = [
  test_case "date" `Quick begin fun _ () ->
    RPC.call_service
      ~base all_media_types V2.date () () () >>= fun (_, _, res) ->
    match res with
    | `Ok (Some date) ->
      Format.(printf "Date: %a@." (pp_print_spaced_list pp_print_float) date) ;
      Lwt.return_unit
    | #RPC.service_result -> Lwt.fail_with ""
  end ;
  test_case "operations" `Quick begin fun _ () ->
    RPC.call_service
      ~base all_media_types V3.operations
      ((), "BLbdSDSwUiEbfUKq53s1V9JwtoxjnTRRqTw3KQkSJ3FuRgtMvTF")
      (Some 50) () >>= fun (_, _, res) ->
    match res with
    | `Ok (Some ops) ->
      Format.(printf "Operations: got %d results" (List.length ops)) ;
      Lwt.return_unit
    | `Unexpected_content (_, err_str) ->
      Lwt.fail_with err_str
    | #RPC.service_result ->
      Lwt.fail_with "unknown error"
  end ;
]

let () =
  run "tzscan" [
    "basic", basic ;
  ]
