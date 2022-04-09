(** Some buffer to hold data, and to read and write data *)

open StdLabels
open Field

type t =
  { mutable offset : int
  ; end_offset : int
  ; data : String.t
  }

let create ?(offset = 0) ?length data =
  let end_offset =
    match length with
    | None -> String.length data
    | Some l -> l + offset
  in
  assert (String.length data >= end_offset);
  { offset; end_offset; data }
;;

(** Return an error if there is not enough data in input *)
let validate_capacity t count =
  if t.offset + count > t.end_offset then raise Common.Reader_too_short
;;

(** Test if there is more data in the buffer to be read *)
let has_more t = t.offset < t.end_offset

let read_byte t =
  validate_capacity t 1;
  let v = t.data.[t.offset] in
  t.offset <- t.offset + 1;
  Char.code v
;;

let rec read_raw_varint_int_aux t acc shift_count =
  let v = read_byte t in
  let acc = Int.(logor acc (shift_left (v land 0x7f) shift_count)) in
  if v < 0x80 then acc else read_raw_varint_int_aux t acc (shift_count + 7)
;;

let read_raw_varint_int t = read_raw_varint_int_aux t 0 0

let rec read_raw_varint_aux t acc shift_count =
  (* TODO (Anurag): Add tests for this *)
  let v = read_byte t in
  let acc = Int64.(logor acc (shift_left (of_int (v land 0x7f)) shift_count)) in
  if v < 0x80 then acc else read_raw_varint_aux t acc (shift_count + 7)
;;

let read_varint t = Varint (read_raw_varint_aux t 0L 0)

let read_length_delimited t =
  let length = read_raw_varint_int t in
  validate_capacity t length;
  let v = Length_delimited { offset = t.offset; length; data = t.data } in
  t.offset <- t.offset + length;
  v
;;

(* Implement little endian ourselves *)
let read_fixed32 t =
  let size = 4 in
  validate_capacity t size;
  let v = Bytes.get_int32_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  Fixed_32_bit v
;;

let read_fixed64 t =
  let size = 8 in
  validate_capacity t size;
  let v = Bytes.get_int64_le (Bytes.unsafe_of_string t.data) t.offset in
  t.offset <- t.offset + size;
  Fixed_64_bit v
;;

let read_field t =
  let v = read_raw_varint_int t in
  let field_type = v land 0x7 in
  let field_number = v / 8 in
  let field =
    match field_type with
    | 0 -> read_varint t
    | 1 -> read_fixed64 t
    | 2 -> read_length_delimited t
    | 5 -> read_fixed32 t
    | n -> raise_notrace (Common.Unknown_field_type n)
  in
  field_number, field
;;

let to_list t =
  let rec inner acc =
    match has_more t with
    | true ->
      let v = read_field t in
      inner (v :: acc)
    | false -> List.rev acc
  in
  inner []
;;
