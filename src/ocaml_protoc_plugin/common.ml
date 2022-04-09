exception Reader_too_short
exception Unknown_field_type of int
exception Wrong_field_type of (string * Field.t)
exception Illegal_value of (string * Field.t)
exception Required_field_missing
exception Unknown_enum_value of int