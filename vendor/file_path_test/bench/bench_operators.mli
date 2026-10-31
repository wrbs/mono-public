open! Core

module%template [@alloc a = (heap, stack)] O :
  File_path.Operators [@alloc a] with module Types := File_path.Types
