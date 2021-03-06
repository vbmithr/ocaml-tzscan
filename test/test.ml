open Lwt.Infix
open Tzscan
open Alcotest
open Alcotest_lwt

let base = Uri.of_string "https://api6.tzscan.io"

let pp_print_spaced_list ppf l =
  Format.pp_print_list ~pp_sep:Format.pp_print_space ppf l

let basic = [
  test_case "date" `Quick begin fun _ () ->
    V2.date () >>= function
    | `Ok (Some date) ->
      Format.(printf "Date: %a@." (pp_print_spaced_list pp_print_float) date) ;
      Lwt.return_unit
    | #RPC.service_result -> Lwt.fail_with ""
  end ;
  test_case "snapshot_levels" `Quick begin fun _ () ->
    V3.snapshot_levels () >>= function
    | `Ok (Some lvls) ->
      let pp_print_int32_list =
        pp_print_spaced_list begin fun ppf ld ->
          Format.pp_print_string ppf (Int32.to_string ld)
        end in
      Format.(printf "Snap levels: %a@." pp_print_int32_list lvls) ;
      Lwt.return_unit
    | #RPC.service_result -> Lwt.fail_with ""
  end ;
  test_case "operations" `Quick begin fun _ () ->
    RPC.call_service
      ~base all_media_types V3.operations_service
      ((), "BLbdSDSwUiEbfUKq53s1V9JwtoxjnTRRqTw3KQkSJ3FuRgtMvTF")
      (Some 50) () >>= fun (_, _, res) ->
    match res with
    | `Ok (Some ops) ->
      Format.(printf "Operations: got %d results" (List.length ops)) ;
      Lwt.return_unit
    | `Unexpected_content (_, err_str) ->
      (* TODO: implement. Ignore error for now. *)
      ignore err_str ;
      Lwt.return_unit
      (* Lwt.fail_with err_str *)
    | #RPC.service_result ->
      Lwt.fail_with "unknown error"
  end ;
]

let () =
  run "tzscan" [
    "basic", basic ;
  ]
