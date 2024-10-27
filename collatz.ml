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

open Cmdliner
open Generate
module Log = Owl.Log
module G = Bigarray.Array1

let generate (size : int) (output : string option) =
  Log.info
    "Collatz Dataset Generator: computing the Collatz steps for the first \
     %d-th natural numbers."
    size;
  let time = Unix.gettimeofday () in
  let memory, db = generate size in
  Log.info "Done in %.0f seconds." (Unix.time () -. time);
  let mean, variance = Dataset.var_mean db 16 in
  Log.info "Mean %f, Variance %f" mean variance;

  (match output with
  | Some filename ->
      Log.info "Saving data to %s" filename;
      Dataset.csv db filename
  | None -> ());

  Dataset.close db;
  Memory.close memory;
  `Ok ()

let export output =
  Log.info "Exporting data to %s" output;
  let db = Dataset.create 0 in
  Dataset.csv db output;
  Dataset.close db;
  `Ok ()

let size_arg =
  let doc = "The size of the dataset to generate." in
  Arg.(required & pos 0 (some int) None & info [] ~docv:"SIZE" ~doc)

let output_arg =
  let doc = "The output file to save the generated data." in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)

let output_arg_export =
  let doc = "The output CSV file." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"OUTPUT" ~doc)

let generate_cmd =
  let doc = "Generate the Collatz dataset." in
  let man =
    [
      `S Manpage.s_description;
      `P "Generate the Collatz dataset for the specified size.";
    ]
  in
  ( Term.(ret (const generate $ size_arg $ output_arg)),
    Cmd.info "generate" ~doc ~man )

let export_cmd =
  let doc = "Export the dataset to a CSV file." in
  let man =
    [
      `S Manpage.s_description;
      `P "Export the dataset from the specified input file to a CSV file.";
    ]
  in
  (Term.(ret (const export $ output_arg_export)), Cmd.info "export" ~doc ~man)

let cmds =
  [
    Cmd.v (snd generate_cmd) (fst generate_cmd);
    Cmd.v (snd export_cmd) (fst export_cmd);
  ]

let cmd =
  let doc = "Generates and exports Collatz steps dataset." in
  let man =
    [
      `S Manpage.s_description;
      `P "Generates and exports the Collatz steps dataset.";
      `S Manpage.s_bugs;
      `P "Report bugs to <gaby.santamaria@outlook.fr>.";
    ]
  in
  let info = Cmd.info "collatz" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.group info cmds

let main () = exit (Cmd.eval cmd)
let () = main ()
