include module type of Ppx_yojson_conv_lib.Yojsonable

module Of_yojsonable : sig
  module V1
      (Yojsonable : S)
      (M : sig
         type t

         val of_yojsonable : Yojsonable.t -> t
         val to_yojsonable : t -> Yojsonable.t
       end) : S with type t := M.t
end

module Of_stringable : sig
  module V1 (M : Base.Stringable.S) : S with type t := M.t
end
