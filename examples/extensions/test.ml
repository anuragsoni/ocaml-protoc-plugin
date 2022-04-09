open Extensions

(* Set extensions *)
let _ =
  let foo = Foo.{ i = 31; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo_with_bar = Bar.set foo (Some 42) in
  let foo_with_baz = Baz.set foo (Some "Test String") in
  let foo_with_bar_baz = Baz.set foo_with_bar (Some "Test String") in
  (* Get extensions *)
  let bar = Bar.get foo_with_bar in
  let baz = Baz.get foo_with_baz in
  assert (bar = Some 42);
  assert (baz = Some "Test String");
  let bar' = Bar.get foo_with_bar_baz in
  let baz' = Baz.get foo_with_bar_baz in
  assert (bar' = Some 42);
  assert (baz' = Some "Test String");
  ()
;;
