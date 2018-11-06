open Lwt.Infix
open Tzscan
open Alcotest
open Alcotest_lwt

let base = Uri.of_string "https://api1.tzscan.io"

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
    | _ -> Lwt.fail_with ""
  end ;
]

let () =
  run "tzscan" [
    "basic", basic ;
  ]
