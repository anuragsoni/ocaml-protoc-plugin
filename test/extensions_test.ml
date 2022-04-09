open Extensions

let%expect_test _ =
  let foo =
    Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default }
  in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let baz = Extensions.Baz'.get foo in
  print_endline ([%show: Extensions.Baz.t] baz);
  let () =
    match baz = Some 7 with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| (Some 7) |}]
;;

let%expect_test _ =
  let foo =
    Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default }
  in
  let foo = Extensions.Baz'.set foo (Some 8) in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let baz = Extensions.Baz'.get foo in
  print_endline ([%show: Extensions.Baz.t] baz);
  let () =
    match baz = Some 7 with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| (Some 7) |}]
;;

let%expect_test _ =
  let foo =
    Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default }
  in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let foo' =
    Extensions.Foo.to_proto foo
    |> Ocaml_protoc_plugin.Writer.contents
    |> Ocaml_protoc_plugin.Reader.create
    |> Extensions.Foo.from_proto
  in
  let baz = Extensions.Baz'.get foo' in
  print_endline ([%show: Extensions.Baz.t] baz);
  let () =
    match baz = Some 7 with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| (Some 7) |}]
;;

let%expect_test _ =
  let v = [ 6; 7; 8; 9 ] in
  let foo =
    Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default }
  in
  let foo = Extensions.R_baz.set foo v in
  let foo' =
    Extensions.Foo.to_proto foo
    |> Ocaml_protoc_plugin.Writer.contents
    |> Ocaml_protoc_plugin.Reader.create
    |> Extensions.Foo.from_proto
  in
  let r_baz = Extensions.R_baz.get foo' in
  print_endline ([%show: Extensions.R_baz.t] r_baz);
  let () =
    match r_baz = v with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| [6; 7; 8; 9] |}]
;;
