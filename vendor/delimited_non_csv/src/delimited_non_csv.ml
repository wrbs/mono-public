(** Parsers for non-CSV-like formats *)

(** [Character_separated_without_quoting] parses fields separated by a character, where
    fields may contain escaped characters (e,g, [\n]) but fields may not be quoted (e.g.,
    ["foo bar"]). *)
module Character_separated_without_quoting = Character_separated_without_quoting

(** [Positional] parses fixed-width fields. *)
module Positional = Positional
