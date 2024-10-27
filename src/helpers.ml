(*****************************************************************************)
(*                                                                           *)
(*                                                                           *)
(*  Copyright (C) 2024                                                       *)
(*    Gabriel Santamaria                                                     *)
(*                                                                           *)
(*                                                                           *)
(*  Licensed under the Apache License, Version 2.0 (the "License");          *)
(*  you may not use this file except in compliance with the License.         *)
(*  You may obtain a copy of the License at                                  *)
(*                                                                           *)
(*    http://www.apache.org/licenses/LICENSE-2.0                             *)
(*                                                                           *)
(*  Unless required by applicable law or agreed to in writing, software      *)
(*  distributed under the License is distributed on an "AS IS" BASIS,        *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(*  See the License for the specific language governing permissions and      *)
(*  limitations under the License.                                           *)
(*                                                                           *)
(*****************************************************************************)

let bytes_to_string n =
  let suffixes = [| "b"; "Kb"; "Mb"; "Gb"; "Tb" |] in
  let rec find_suffix n idx =
    if n < 1000 || idx = Array.length suffixes - 1 then
      Printf.sprintf "%d%s" n suffixes.(idx)
    else find_suffix (n / 1000) (idx + 1)
  in
  find_suffix n 0
