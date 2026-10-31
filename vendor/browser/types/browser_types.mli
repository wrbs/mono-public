(* To generate bindings for a new interface or namespace, just add the name (in
   PascalCase) to allow list.sexp in [../common_files]!

   The allow list is automatically sorted, deduplicated, and validated:
   - Non-interface/namespace entries are filtered out
   - Non-existent interfaces/namespaces are filtered out
   - Interfaces must have their ancestors in the allow list (raises exception if not)
   - Interfaces with unsafe extended attributes are rejected (see
     [Translate.translate_extended_attributes] for full list of disallowed extended
     attributes.)

   Referenced dictionaries, enums, typedefs, and callbacks are emitted automatically.
   Referenced interfaces require explicit allow list entry to be emitted. *)
include module type of Enums
include module type of Interfaces
include module type of Types
include module type of Type_casters
include module type of Namespaces
module Classes = Class_definitions
module Globals = Global_scopes
