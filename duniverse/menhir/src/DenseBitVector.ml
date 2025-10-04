(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module A =
  AtomicBitSet

type data =
  A.t array

type vector =
  data ref

type word =
  A.t

let create () =
  ref [|A.empty|]

let[@inline] length v =
  let data = !v in
  Array.length data

let grow v a =
  let data = !v in
  let len = Array.length data in
  if a < len then
    ()
  else
    let rlen = ref len in
    while !rlen <= a do rlen := !rlen * 2 done;
    let data' = Array.make !rlen A.empty in
    Array.blit data 0 data' 0 len;
    v := data'

let[@inline] get v a =
  let data = !v in
  data.(a)

let[@inline] set v a w =
  let data = !v in
  data.(a) <- w

let width v =
  let data = !v in
  (* Starting from the end, search for the last nonzero word. *)
  let last = ref (Array.length data - 1) in
  while !last >= 0 && A.is_empty data.(!last) do decr last done;
  if !last < 0 then
    (* All words are zero. The vector has zero width. *)
    0
  else
    (* Return the index of the most significant bit that is set, plus one. *)
    let i = !last * A.bound + A.maximum data.(!last) in
    i + 1
