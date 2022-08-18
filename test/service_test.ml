open Service

let service reader =
  let s_deser, s_ser =
    Ocaml_protoc_plugin.Service.make_service_functions (module Service.String_of_int.Call)
  in
  let req = s_deser reader in
  string_of_int req |> s_ser
;;

let call i =
  let c_ser, c_deser =
    Ocaml_protoc_plugin.Service.make_client_functions (module Service.String_of_int.Call)
  in
  let req = i in
  req
  |> c_ser
  |> Ocaml_protoc_plugin.Writer.contents
  |> Ocaml_protoc_plugin.Reader.create
  |> service
  |> Ocaml_protoc_plugin.Writer.contents
  |> Ocaml_protoc_plugin.Reader.create
  |> c_deser
;;

let%expect_test _ =
  Caml.Printf.printf "%d -> \"%s\"\n" 0 (call 0);
  Caml.Printf.printf "%d -> \"%s\"\n" 5 (call 5);
  Caml.Printf.printf "%d -> \"%s\"\n" 50 (call 50);
  Caml.Printf.printf "%d -> \"%s\"\n" (-5) (call (-5));
  Caml.Printf.printf "%d -> \"%s\"\n" (-100) (call (-100));
  ();
  [%expect
    {|
    0 -> "0"
    5 -> "5"
    50 -> "50"
    -5 -> "-5"
    -100 -> "-100" |}]
;;
