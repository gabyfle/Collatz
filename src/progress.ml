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

type t = {
  total : int;
  current : int Atomic.t;
  starting_time : float;
  mutable last_update : float;
  update_interval : float;
}

let create total =
  {
    total;
    current = Atomic.make 0;
    starting_time = Unix.gettimeofday ();
    last_update = Unix.gettimeofday ();
    update_interval = 0.9;
  }

let add t =
  let new_count = Atomic.fetch_and_add t.current 1 in
  let now = Unix.gettimeofday () in
  if now -. t.last_update >= t.update_interval then
    (t.last_update <- now;
     let percentage =
       float_of_int (new_count + 1) /. float_of_int t.total *. 100.
     in
     (* Clear line and update progress *)
     Printf.printf
       "\r\027[K[\027[1;32m%3.0f%%\027[0m] Processing: %d/%d (\027[1;33m%.2f \
        OP/s\027[0m)%!"
       percentage (new_count + 1) t.total)
      (float_of_int (new_count + 1) /. (now -. t.starting_time));
  flush stdout

let finish t =
  Printf.printf
    "\r\027[K[\027[1;32m100%%\027[0m] Completed: %d/%d%! (\027[1;33m%.2f \
     OP/s\027[0m)\n"
    t.total t.total
    (float_of_int t.total /. (Unix.gettimeofday () -. t.starting_time));
  flush stdout
