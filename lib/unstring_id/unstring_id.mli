open! Core

module type S = sig
  include String_id.S_with_extras [@mode local]

  val unstring : t Unstring.t
end

val make
  :  ?quickcheck_generator:string Quickcheck.Generator.t
  -> unit Unstring.t
  -> module_name:string
  -> caller_identity:Bin_shape.Uuid.t
  -> (module S)
