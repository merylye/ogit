(** Raw interface to git system commands *)
module type Plumbing = sig
  (** Representation of raw git commands.

      This module represents the calling of calling git commands with
      any command line arguments *)

  (** The representation of the result of executing a git command *)
  type result

  (** [make result o e] is a result where [o] is the lines of stdout and
      [e] is the lines of stderr *)
  val make_result : string list -> string list -> string list -> result

  (** [get_stdout r] is the lines of stdout *)
  val get_stdout : result -> string list

  (** [get_stdout r] is the lines of stderr *)
  val get_stderr : result -> string list

  (** [get_out r] is the lines of of both stdout and stderr in the order
      that they were sent to their respective streams. For example, if
      data was written to stdout, then stderr, and then stdout, again,
      then [get_out r] follows that same order. *)
  val get_out : result -> string list

  (** [init args] calls git init with arguments [args] *)
  val init : string array -> result

  (** [push] calls git push with arguments [args]*)
  val push : string array -> result

  (** [pull args] calls git pull with arguments [args] *)
  val pull : string array -> result

  (** [hash_object args] calls git hash-object with arguments [args] and
      is the output to standard output *)
  val hash_object : string array -> result

  (** [cat_file args] calls git cat-file with arguments [args] *)
  val cat_file : string array -> result

  (** [update_index args] calls git update-index with arguments [args] *)
  val update_index : string array -> result

  (** [write_tree args] calls git write-tree with arguments [args] *)
  val write_tree : string array -> result

  (** [read_tree args] calls git read-tree with arguments [args] *)
  val read_tree : string array -> result

  (** [commit_tree args] calls git commit-tree with arguments [args] *)
  val commit_tree : string array -> result

  (** [log args] calls git log with arguments [args] *)
  val log : string array -> result

  (** [add args] calls git add with arguments [args] *)
  val add : string array -> result

  (** [restore args] calls git restore with arguments [args] *)
  val restore : string array -> result

  (** [commit] calls git commit with arguments [args] *)
  val commit : string array -> result

  (** [show args] calls git show with arguments [args] *)
  val show : string array -> result

  (** [diff args] calls git diff with arguments [args] *)
  val diff : string array -> result

  (** [revparse args] calls git rev-parse with arguments [args] *)
  val revparse : string array -> result

  (** [status args] calls git status with arguments [args] *)
  val status : string array -> result

  (** [head args] calls git symbolic-ref HEAD with arguments [args]*)
  val head : string array -> result

  (** [checkout args] calls git checkout with arguments [args]*)
  val checkout : string array -> result

  (** [branch args] calls git branch with arguments [args] *)
  val branch : string array -> result

  (** [stash args] calls git stash with arguments [args] *)
  val stash : string array -> result

  (** [reset args] calls git reset with arguments [args] *)
  val reset : string array -> result

  (** [git args] calls git with arguments [args] *)
  val git : string array -> result
end

module type PlumbingWithSet = sig
  include Plumbing

  (**[set_log_data out err out_and_err changes the log to a result containing \[out\], \[err\], and \[out_and_err\]]*)
  val set_log_data : string list -> string list -> string list -> unit

  (**[set_status_data out err out_and_err] changes the status to a
     result containing [out], [err], and [out_and_err]*)
  val set_status_data :
    string list -> string list -> string list -> unit

  (**[set_head_data out err out_and_err] changes the head to a result
     containing [out], [err], and [out_and_err]*)
  val set_head_data : string list -> string list -> string list -> unit
end

(** The [Plumbing] used to run OGit *)
module ProdPlumbing : Plumbing = struct
  (** Types and methods to access and construct the type *)
  type result = {
    stdout : string list;
    stderr : string list;
    out_and_err : string list; (*exit_code : int;*)
  }

  (** [make result o e] is a result where [o] is the lines of stdout and
      [e] is the lines of stderr *)
  let make_result out err out_and_err =
    { stdout = out; stderr = err; out_and_err }

  (** [get_stdout r] is the lines of stdout *)
  let get_stdout result = result.stdout

  (** [get_stderr r] is the lines of stderr *)
  let get_stderr result = result.stderr

  (** [get_out r] is the lines of of both stdout and stderr in the order
      that they were sent to their respective streams.

      For example, if data was written to stdout, then stderr, and then
      stdout, again, then [get_out r] follows that same order. *)
  let get_out result = result.out_and_err

  (** Helper Methods *)

  (** [read fd] is the lines of file referenced by descriptor [fd] *)
  let read (fd : Unix.file_descr) : string list =
    let in_ch = Unix.in_channel_of_descr fd in
    let lines = ref [] in
    try
      while true do
        lines := input_line in_ch :: !lines
      done;
      !lines
    with End_of_file ->
      close_in in_ch;
      !lines

  (** [fork_and_execv e a] is the result of executing program [exe] with
      arguments [args]*)
  let fork_and_execv (exe : string) (args : string array) : result =
    let inp_stdout, out_stdout = Unix.pipe () in
    (* Pipe for stdout *)
    let inp_stderr, out_stderr = Unix.pipe () in
    (* Pipe for stderr *)
    let inp, out = Unix.pipe () in
    (* Pipe for both stdout and stderr *)
    let pid = Unix.fork () in
    if pid = 0 then (
      Unix.close inp_stdout;
      (* Not used by child *)
      Unix.close inp_stderr;
      (* Not used by child *)
      Unix.close inp;
      (* Not used by child *)
      Unix.dup2 out_stdout Unix.stdout;
      (* Bind stdout pipe to stdout *)
      Unix.dup2 out_stderr Unix.stderr;
      (* Bind stderr pipe to stderr *)
      Unix.dup2 out Unix.stdout;
      (* Bind out pipe to stdout *)
      Unix.dup2 out Unix.stderr;
      (* Bind out pipe to stderr *)
      Unix.execvp exe args)
    else (
      Unix.close out_stdout;
      (* Not used by parent*)
      Unix.close out_stderr;
      (* Not used by parent*)
      Unix.close out;
      (* Not used by parent*)
      let stdout = read inp_stdout in
      let stdin = read inp_stderr in
      let out_and_err = read inp in
      (* Does not close the pipes because [read fd] does that when it
         closes the input channel it creates.

         Unix.close inp_stderr; Unix.close inp_stdout; Unix.close inp; *)
      make_result stdout stdin out_and_err)

  (** [init args] calls git init with arguments [args] *)
  let init (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "init" |] args)

  (** [push] calls git push with arguments [args]*)
  let push (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "push" |] args)

  (** [pull args] calls git pull with arguments [args] *)
  let pull (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "pull" |] args)

  (** [hash_object args] calls git hash-object with arguments [args] and
      is the output to standard output *)
  let hash_object (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "hash-object" |] args)

  (** [cat_file args] calls git cat-file with arguments [args] *)
  let cat_file (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "cat-file" |] args)

  (** [update_index args] calls git update-index with arguments [args] *)
  let update_index (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "update-index" |] args)

  (** [write_tree args] calls git write-tree with arguments [args] *)
  let write_tree (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "write-tree" |] args)

  (** [read_tree args] calls git read-tree with arguments [args] *)
  let read_tree (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "read-tree" |] args)

  (** [commit_tree args] calls git commit-tree with arguments [args] *)
  let commit_tree (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "commit-tree" |] args)

  (** [log args] calls git log with arguments [args] *)
  let log (args : string array) =
    fork_and_execv "git"
      (Array.append
         [| "git"; "--no-pager"; "log"; "--format=reference" |]
         args)

  (** [add args] calls git add with arguments [args] *)
  let add (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "add" |] args)

  (** [restore args] calls git restore with arguments [args] *)
  let restore (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "restore" |] args)

  (** [commit] calls git commit with arguments [args] *)
  let commit (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "commit" |] args)

  (** [show args] calls git show with arguments [args] *)
  let show (args : string array) =
    fork_and_execv "git"
      (Array.append [| "git"; "--no-pager"; "show" |] args)

  (** [diff args] calls git diff with arguments [args] *)
  let diff (args : string array) =
    fork_and_execv "git"
      (Array.append [| "git"; "--no-pager"; "diff" |] args)

  (** [revparse args] calls git rev-parse with arguments [args] *)
  let revparse (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "rev-parse" |] args)

  (** [status args] calls git status with arguments [args] *)
  let status (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "status" |] args)

  (** [head args] calls git symbolic-ref HEAD with arguments [args]*)
  let head (args : string array) =
    fork_and_execv "git"
      (Array.append [| "git"; "symbolic-ref"; "HEAD" |] args)

  (** [checkout args] calls git checkout with arguments [args]*)
  let checkout (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "checkout" |] args)

  (** [branch args] calls git branch with arguments [args] *)
  let branch (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "branch" |] args)

  (** [stash args] calls git stash with arguments [args] *)
  let stash (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "stash" |] args)

  (** [reset args] calls git reset with arguments [args] *)
  let reset (args : string array) =
    fork_and_execv "git" (Array.append [| "git"; "reset" |] args)

  (** [git args] calls git with arguments [args] *)
  let git (args : string array) =
    fork_and_execv "git" (Array.append [| "git" |] args)
end

(** A simulated [Plumbing] used in an isolated testing environment *)
module MockPlumbing : PlumbingWithSet = struct
  (** Types and methods to access and construct the type *)
  type result = {
    stdout : string list;
    stderr : string list;
    out_and_err : string list; (*exit_code : int;*)
  }

  (** [get_stdout r] is the lines of stdout *)
  let get_stdout result = result.stdout

  (** [get_stderr r] is the lines of stderr *)
  let get_stderr result = result.stderr

  (** [get_out r] is the lines of of both stdout and stderr in the order
      that they were sent to their respective streams.

      For example, if data was written to stdout, then stderr, and then
      stdout, again, then [get_out r] follows that same order. *)
  let get_out result = result.out_and_err

  (** [make result o e] is a result where [o] is the lines of stdout and
      [e] is the lines of stderr *)
  let make_result out err out_and_err =
    { stdout = out; stderr = err; out_and_err }

  (** [git args] simulates calling git with arguments [args] *)
  let git (args : string array) =
    make_result (Array.to_list args) [] (Array.to_list args)

  (** [init args] simulates calling git init with arguments [args] *)
  let init (args : string array) =
    make_result
      [ "Initialized empty Git repository in /home/fake/.git/" ]
      []
      [ "Initialized empty Git repository in /home/fake/.git/" ]

  (** [push] simulates calling git push with arguments [args]*)
  let push (args : string array) =
    let new_args = Array.of_list ("push" :: Array.to_list args) in
    git new_args

  (** [pull args] simulates calling git pull with arguments [args] *)
  let pull (args : string array) =
    let new_args = Array.of_list ("pull" :: Array.to_list args) in
    git new_args

  (** [hash_object args] simulates calling git hash-object with
      arguments [args] and is the output to standard output *)
  let hash_object (args : string array) =
    let new_args =
      Array.of_list ("hash_object" :: Array.to_list args)
    in
    git new_args

  (** [cat_file args] simulates calling git cat-file with arguments
      [args] *)
  let cat_file (args : string array) =
    let new_args = Array.of_list ("cat_file" :: Array.to_list args) in
    git new_args

  (** [branch args] simulates calling git branch with arguments [args] *)
  let branch (args : string array) =
    let new_args = Array.of_list ("cat_file" :: Array.to_list args) in
    git new_args

  (** [update_index args] simulates calling git update-index with
      arguments [args] *)
  let update_index (args : string array) =
    let new_args =
      Array.of_list ("update-index" :: Array.to_list args)
    in
    git new_args

  (** [write_tree args] simulates calling git write-tree with arguments
      [args] *)
  let write_tree (args : string array) =
    let new_args = Array.of_list ("write-tree" :: Array.to_list args) in
    git new_args

  (** [read_tree args] simulates calling git read-tree with arguments
      [args] *)
  let read_tree (args : string array) =
    let new_args = Array.of_list ("read-tree" :: Array.to_list args) in
    git new_args

  (** [commit_tree args] simulates calling git commit-tree with
      arguments [args] *)
  let commit_tree (args : string array) =
    let new_args =
      Array.of_list ("commit-tree" :: Array.to_list args)
    in
    git new_args

  (**[log_data] is the commit history log in [result] form*)
  let log_data =
    ref
      {
        stdout =
          [
            "59689ce (setup project files, 2021-03-22)";
            "b92c19e (Initial commit, 2021-03-04)";
          ];
        stderr = [];
        out_and_err =
          [
            "59689ce (setup project files, 2021-03-22)";
            "b92c19e (Initial commit, 2021-03-04)";
          ];
      }

  (**[set_log_data out err out_and_err changes the log to a result containing 
  \[out\], \[err\], and \[out_and_err\]]*)
  let set_log_data out err out_and_err =
    log_data := make_result out err out_and_err

  (** [log args] simulates calling git log with arguments [args] *)
  let log (args : string array) = !log_data

  (**[status_data] is the result representing the current git status*)
  let status_data = ref { stdout = []; stderr = []; out_and_err = [] }

  (** [add args] simulates calling git add with arguments [args] *)
  let add (args : string array) =
    let new_args = Array.of_list ("add" :: Array.to_list args) in
    let exec = git new_args in
    set_log_data (Array.to_list args) [] (Array.to_list args);
    exec

  (** [stash args] simulates calling git stash with arguments [args] *)
  let stash (args : string array) =
    let new_args = Array.of_list ("stash" :: Array.to_list args) in
    git new_args

  (** [head args] simulates calling git symbolic-ref HEAD with arguments
      [args]*)
  let head (args : string array) =
    let new_args = Array.of_list ("head" :: Array.to_list args) in
    git new_args

  (**[checkout args] simulates calling [git checkout] with arguments
     [args]*)
  let checkout (args : string array) =
    let new_args = Array.of_list ("checkout" :: Array.to_list args) in
    git new_args

  (** [restore args] simulates calling git restore with arguments [args] *)
  let restore (args : string array) =
    let new_args = Array.of_list ("restore" :: Array.to_list args) in
    git new_args

  (** [commit] simulates calling git commit with arguments [args] *)
  let commit (args : string array) =
    let new_args = Array.of_list ("commit" :: Array.to_list args) in
    git new_args

  (** [show args] simulates calling git show with arguments [args] *)
  let show (args : string array) =
    let new_args = Array.of_list ("show" :: Array.to_list args) in
    git new_args

  (** [diff args] simulates calling git diff with arguments [args] *)
  let diff (args : string array) =
    let new_args = Array.of_list ("diff" :: Array.to_list args) in
    git new_args

  (**[head_data] is the information about the head branch*)
  let head_data =
    ref
      {
        stdout = [ "origin/master" ];
        stderr = [];
        out_and_err = [ "origin/master" ];
      }

  (**[set_head_data out err out_and_err] changes the head to a result
     containing [out], [err], and [out_and_err]*)
  let set_head_data out err out_and_err =
    head_data := make_result out err out_and_err

  let head (args : string array) = !head_data

  (** [checkout args] simulates calling git checkout with arguments
      [args]*)
  let checkout (args : string array) =
    let new_args = Array.of_list ("push" :: Array.to_list args) in
    git new_args

  (** [reset args] simulates calling git reset with arguments [args] *)
  let reset (args : string array) =
    let new_args = Array.of_list ("reset" :: Array.to_list args) in
    git new_args

  (** [revparse args] simulates calling git rev-parse with arguments
      [args] *)
  let revparse (args : string array) =
    {
      stdout = [ "origin/master" ];
      stderr = [];
      out_and_err = [ "origin/master" ];
    }

  (**[set_status_data out err out_and_err] changes the status to a
     result containing [out], [err], and [out_and_err]*)
  let set_status_data out err out_and_err =
    log_data := make_result out err out_and_err;
    status_data := { stdout = out; stderr = []; out_and_err }

  (** [status args] simulates calling git status with arguments [args] *)
  let status (args : string array) = !status_data
end
