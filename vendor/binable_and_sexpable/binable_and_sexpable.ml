open Core
include Binable_and_sexpable_intf

[%%template
[@@@mode.default m = (global, local)]

module%template.portable
  [@modality p] Of_format
    (Format : S
  [@mode m])
    (M : Arg [@mode m] with module Format := Format) =
struct
  include
    Binable.Of_binable_without_uuid [@mode m] [@modality p] [@alert "-legacy"]
      (Format)
      (struct
        type t = M.t

        let[@mode m = (global, m)] to_binable = (M.to_format [@mode m])
        let of_binable = M.of_format
      end)

  include
    Sexpable.Of_sexpable [@modality p]
      (Format)
      (struct
        type t = M.t

        let to_sexpable = M.to_format
        let of_sexpable = M.of_format
      end)
end

[@@@kind.default ka = (value, any)]

module%template.portable
  [@modality p] Of_format1
    (Format : S1
  [@kind ka] [@mode m])
    (M : Arg1 [@kind ka] [@mode m] with module Format := Format) =
struct
  include
    Binable.Of_binable1_without_uuid [@kind ka] [@mode m] [@modality p] [@alert "-legacy"]
      (Format)
      (struct
        type ('a : ka) t = 'a M.t

        let[@mode m = (global, m)] to_binable = (M.to_format [@mode m])
        let of_binable = M.of_format
      end)

  include
    Sexpable.Of_sexpable1 [@kind ka] [@modality p]
      (Format)
      (struct
        type ('a : ka) t = 'a M.t

        let to_sexpable = M.to_format
        let of_sexpable = M.of_format
      end)
end

[@@@kind.default kb = (value, any)]

module%template.portable
  [@modality p] Of_format2
    (Format : S2
  [@kind ka kb] [@mode m])
    (M : Arg2 [@kind ka kb] [@mode m] with module Format := Format) =
struct
  include
    Binable.Of_binable2_without_uuid
      [@kind ka kb]
      [@mode m]
      [@modality p]
      [@alert "-legacy"]
      (Format)
      (struct
        type ('a : ka, 'b : kb) t = ('a, 'b) M.t

        let[@mode m = (global, m)] to_binable = (M.to_format [@mode m])
        let of_binable = M.of_format
      end)

  include
    Sexpable.Of_sexpable2 [@kind ka kb] [@modality p]
      (Format)
      (struct
        type ('a : ka, 'b : kb) t = ('a, 'b) M.t

        let to_sexpable = M.to_format
        let of_sexpable = M.of_format
      end)
end]
