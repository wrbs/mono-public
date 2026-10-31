open! Core
module Path : File_path.Common with module Type := File_path.Types.Path
module Absolute : File_path.Common with module Type := File_path.Types.Absolute
module Relative : File_path.Common with module Type := File_path.Types.Relative
module Part : File_path.Common with module Type := File_path.Types.Part
