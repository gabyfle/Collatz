open Domainslib
open Lmdb
open Cmdliner
module Log = Owl.Log
module G = Bigarray.Array1

let max_array_size = 1_000_000_000

module Helpers = struct
  let bytes_to_string n =
    let suffixes = [| "b"; "Kb"; "Mb"; "Gb"; "Tb" |] in
    let rec find_suffix n idx =
      if n < 1000 || idx = Array.length suffixes - 1 then
        Printf.sprintf "%d%s" n suffixes.(idx)
      else find_suffix (n / 1000) (idx + 1)
    in
    find_suffix n 0
end

module Progress = struct
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
end

module type TData = sig
  type t

  val create : int -> t
  val close : t -> unit
  val get : t -> int -> int
  val set : t -> int -> int -> unit
  val mem : t -> int -> bool
end

(* Collatz dataset generator *)
module Dataset : sig
  include TData

  val set_batch :
    t -> int -> (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t -> unit

  val var_mean : t -> int -> float * float
  val csv : t -> string -> unit
end = struct
  type t = (Int.t, Int.t, [ `Uni ]) Lmdb.Map.t * Env.t * int * int

  let create (size : int) : t =
    let map_size = 1099511627776 in
    (* 1TB, see LMDB for doc *)
    let str =
      if Sys.file_exists "/tmp/dataset.db" then
        "Opening the existing dataset..."
      else
        Printf.sprintf "Creating the dataset with initial size of %s"
          (Helpers.bytes_to_string map_size)
    in
    Log.warn "%s" str;

    let env =
      Env.(
        create Rw ~flags:Flags.no_subdir "/tmp/dataset.db" ~map_size ~max_maps:1)
    in

    let map =
      Map.(create Nodup ~key:Conv.int32_le_as_int ~value:Conv.int32_le_as_int)
        env
    in
    (map, env, size, map_size)

  let size ((_, _, n, _) : t) = n

  let close (data : t) : unit =
    let _, env, _, _ = data in
    Env.close env

  let get (data : t) (i : int) : int =
    let map, _, _, _ = data in
    Map.get map i

  let set (data : t) (i : int) (value : int) : unit =
    let map, _, _, _ = data in
    Map.set map i value

  let set_batch (data : t) (size : int)
      (batch : (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t) : unit =
    let map, env, _, _ = data in
    let rw = Lmdb.Rw in
    let transaction txn =
      for i = 0 to size - 1 do
        let v = G.get batch i in
        Map.set map ~txn ~flags:Map.Flags.none i v
      done;
      ()
    in
    let r = Txn.go rw env (fun txn -> transaction txn) in
    match r with
    | None ->
        Log.fatal "[Dataset] Error while setting batch. Transaction aborted"
    | Some _ -> ()

  let mem (data : t) i =
    let map, _, _, _ = data in
    try Map.get map i <> 0 with _ -> false

  (* Reference: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm *)
  let partial (data : t) start len =
    let mean = ref 0. in
    let m2 = ref 0. in
    let n = ref 0 in
    for i = start to start + len - 1 do
      incr n;
      try
        let j = Int.to_float (get data i) in
        let delta = j -. !mean in
        mean := !mean +. (delta /. float_of_int !n);
        m2 := !m2 +. (delta *. (j -. !mean))
      with Not_found -> Printf.printf "Not found %d\n" i
    done;
    (!mean, !m2, !n)

  let combine mean m2 n1 mean' m2' n2 =
    let n = n1 + n2 in
    if n = 0 then (0.0, 0.0, 0)
    else
      let delta = mean' -. mean in
      let n1, n2, fn = (float_of_int n1, float_of_int n2, float_of_int n) in
      let mean_combined = ((n1 *. mean) +. (n2 *. mean')) /. fn in
      let m2_combined = m2 +. m2' +. (n1 *. n2 *. delta *. delta /. fn) in
      (mean_combined, m2_combined, n)

  let var_mean (data : t) (num_domains : int) : float * float =
    let pool = Task.setup_pool ~num_domains () in
    let n = size data in

    let result =
      Task.run pool (fun () ->
          let chunk_size = n / num_domains in
          let tasks =
            List.init num_domains (fun i ->
                let start = if i = 0 then 1 else (i * chunk_size) + 1 in
                let len =
                  if i = num_domains - 1 then n - start else chunk_size
                in
                Task.async pool (fun () -> partial data start len))
          in

          (* Gather results from each task *)
          let partial_results = List.map (Task.await pool) tasks in

          (* Combine the results from all tasks *)
          List.fold_left
            (fun (mean_acc, m2_acc, n_acc) (mean, m2, n) ->
              combine mean_acc m2_acc n_acc mean m2 n)
            (1., 1., 2) partial_results)
    in

    let mean, m2, n = result in
    Task.teardown_pool pool;
    (* Final variance *)
    let variance = m2 /. float_of_int n in
    (mean, variance)

  let csv (data : t) (filename : string) =
    (* outputs data into csv format "n", "steps" *)
    let n = size data in
    let oc = open_out filename in
    Printf.fprintf oc "n,steps\n";
    for j = 1 to n - 1 do
      Printf.fprintf oc "%d, %d\n" j (get data j)
    done;
    close_out oc
end

module Memory : sig
  include TData

  val raw : t -> (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t
end = struct
  type shard = {
    shard_size : int;
    mutexes : Mutex.t Array.t;
    total_size : int; (* Added to handle edge cases *)
  }

  type t = shard * (int, Bigarray.int16_signed_elt, Bigarray.c_layout) G.t

  let create (size : int) : t =
    let arr =
      G.init Bigarray.int16_signed Bigarray.c_layout size (fun _ -> 0)
    in
    let nshards = 16 in
    let base_shard_size = size / nshards in
    let extra_elements = size mod nshards in
    let shard_size = base_shard_size + if extra_elements > 0 then 1 else 0 in
    let shards = Array.init nshards (fun _ -> Mutex.create ()) in
    let shard = { shard_size; mutexes = shards; total_size = size } in
    Log.warn "Memory allocated with size %s"
      (Helpers.bytes_to_string (G.size_in_bytes arr));
    (shard, arr)

  let index (data : t) (i : int) : int =
    let shard = fst data in
    if i >= shard.total_size then raise (Invalid_argument "Index out of bounds");
    min (i / shard.shard_size) (Array.length shard.mutexes - 1)

  let size (data : t) = (fst data).total_size
  let raw (data : t) = snd data
  let close _ = ()

  let get (data : t) (i : int) : int =
    let shard_info = fst data in
    if i >= shard_info.total_size then
      raise (Invalid_argument "Index out of bounds");
    let idx = index data i in
    let mutex = shard_info.mutexes.(idx) in
    Mutex.lock mutex;
    let v = G.get (snd data) i in
    Mutex.unlock mutex;
    v

  let set (data : t) (i : int) (value : int) =
    let shard_info = fst data in
    if i >= shard_info.total_size then
      raise (Invalid_argument "Index out of bounds");
    let idx = index data i in
    let mutex = shard_info.mutexes.(idx) in
    Mutex.lock mutex;
    G.set (snd data) i value;
    Mutex.unlock mutex

  let mem (data : t) (i : int) = i < size data && get data i <> 0
end

let compute_collatz (size : int) =
  let min_size = min size max_array_size in

  Log.info "Creating the environment...";
  let memory = Memory.create (max min_size max_array_size) in
  let db = Dataset.create size in

  let num_domains = Domain.recommended_domain_count () in
  Log.info "Number of domains used for computation: %d." num_domains;
  Log.info "Starting the computation...";

  (* see [1] for the calculation of the third iterate of the C(n) function *)
  let first_values = [| 0; 1; 7; 2; 5; 8; 16; 3 |] in

  let coefs =
    [| (1, 0); (6, 2); (6, 4); (36, 20); (6, 8); (6, 2); (6, 4); (36, 20) |]
  in

  (* this is the number of "in-memory" data allocations we're going to do *)
  let niter =
    if size < max_array_size then 1 else (size / max_array_size) + 1
  in
  let rec collatz n (delta : int) =
    if n - 1 < 8 then first_values.(n - 1)
    else if Memory.mem memory (n - 1) then Memory.get memory (n - 1)
    else if Dataset.mem db n then Dataset.get db n
    else
      let m = n mod 8 in
      let a, b = coefs.(m) in
      let n' = ((a * n) + b) / 8 in
      3 + collatz n' delta
  in

  let process () =
    let progress = Progress.create size in
    for iter = 0 to niter - 1 do
      let delta = min_size * iter in
      let ub = min (min_size - 1) (size - delta) in
      let pool = Task.setup_pool ~num_domains () in

      Task.run pool (fun _ ->
          Task.parallel_for pool ~start:0 ~finish:(ub - 1) ~body:(fun i ->
              let v = collatz (delta + i + 1) delta in
              Memory.set memory i v;
              Progress.add progress));

      Task.teardown_pool pool;
      Progress.finish progress;

      Dataset.set_batch db ub (Memory.raw memory);
      Gc.full_major ()
    done
  in

  process ();
  Gc.full_major ();
  (memory, db)

let generate (size : int) (output : string option) =
  Log.info
    "Collatz Dataset Generator: computing the Collatz steps for the first \
     %d-th natural numbers."
    size;
  let time = Unix.gettimeofday () in
  let memory, db = compute_collatz size in
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
