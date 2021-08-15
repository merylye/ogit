(** A programmer friendly wrapper of git system commands *)

open Plumbing

module type Porcelain = sig
  (** Representation of porcelain git commands.

      This module represents the calling of calling git commands that
      the user interface will find more useful than using the raw
      plumbing commands *)

  (** The abstract type of a git commit object *)
  type commit_t

  (** The type identifying any object *)
  type object_id = string

  (** The abstract type containing the contents of a blob, tree, commit,
      or tag *)
  type object_content

  (** The abstract type determining whether an object is a blob, tree,
      commit, or tag *)
  type object_type

  (** The abstract type represeting the current git state *)
  type status_t

  (** [init d] initializes a git repository in the current working
      directory if [n] is [None], otherwise initializes a git repository
      with name [n] as a subdirectory in the current working directory *)
  val init : string option -> unit

  (** [pull] pulls files from repository *)
  val pull : string -> string -> string -> string

  (** [push] pushes files to the repository *)
  val push : string -> string -> string -> string

  (** [log h] is the list of commit objects that are reachable from HEAD
      in reverse chronological order if [h] is [None], otherwise the
      commit objects that are reachable by following parents of commit
      [h] in reverse chronological order *)
  val log : object_id option -> commit_t list

  (** [add fnames] adds the files with filenames [fnames] to the staging
      area *)
  val add : string list -> unit

  (** [branch_msg n] is the message of the last commit in the branch
      named [n] *)
  val branch_msg : string -> string

  (** [restore_staged fnames] restores staged files [fnames] from the
      staging area *)
  val restore_staged : string list -> unit

  (** [commit msg] commits the changes in the staging area with commit
      message [msg] *)
  val commit : string -> string

  (** [diff] shows the diffs of tracked files *)
  val diff : unit -> string

  (** [status] shows the status of the working tree *)
  val status : unit -> status_t

  (** [checkout b] switches to branch named [b] *)
  val checkout : string -> string

  (** [create_branch b] creates a new branch with name [b] *)
  val create_branch : string -> string

  (** [delete_branch b] deletes the branch named [b] *)
  val delete_branch : string -> string

  (** [stash_apply] applies changes to the current working tree, leaving
      them on the stash stack. *)
  val stash_apply : unit -> string

  (** [stash_pop] applies changes to the current working tree and
      removes them from the stash stack. *)
  val stash_pop : unit -> string

  (** [reset_hard c] puts the head at the commit with hash [c], deleting
      any changes made after [c]. *)
  val reset_hard : string -> string

  (** [reset_soft c] puts the head at the commit with has [c], but keeps
      all changes made after [c]. *)
  val reset_soft : string -> string

  (** [string_of_commit c] is a commit in the form [hash msg] *)
  val string_of_commit_t : commit_t -> string

  (** [get_untracked s] is the untracked filenames in the status [s] *)
  val get_untracked : status_t -> string list

  (** [get_tracked s] is the untracked filenames in the status [s] *)
  val get_tracked : status_t -> string list

  (** [get_staged s] is the staged filenames in the status [s] *)
  val get_staged : status_t -> string list
end

(** The porcelain used when running OGit *)
module PorcelainImpl (P : Plumbing) = struct
  (** The abstract type of a git commit object in [PorcelainImpl]*)
  type commit_t = {
    tree : string;
    (*parents : string;*)
    (*author : string;*)
    (*committer : string;*)
    msg : string;
  }

  (** The type identifying any object in [PorcelainImpl]*)
  type object_id = string

  (** The abstract type containing the contents of a blob, tree, commit,
      or tag in [PorcelainImpl]*)
  type object_content = unit

  (** The abstract type determining whether an object is a blob, tree,
      commit, or tag in [PorcelainImpl]*)
  type object_type =
    | Blob of { contents : string }
    | Tree of {
        entry : int;
        ob_type : object_type;
        sha1 : string;
        name : string;
      }
    | Commit of { com_ob : commit_t }
    | Tag of {
        obj_name : string;
        (*?*)
        ob_type : object_type;
        tagger : string;
        msg : string;
      }

  (** The abstract type represeting the current git state in
      [PorcelainImpl]*)
  type status_t = {
    untracked : string list;
    tracked : string list;
    staged : string list;
  }

  (**[rm_leading_spaces str] returns [str] with any leading spaces
     removed. All characters after the first non-space character are
     left intact.*)
  let rec rm_leading_spaces str =
    match String.split_on_char ' ' str with
    | [] -> str
    | [ "" ] -> str
    | "" :: t ->
        rm_leading_spaces (String.sub str 0 (String.length str - 1))
    | h :: t -> str

  (** [pull u p b] pulls files from branch [b]. [u] is the username and
      [p] is the password of the user. *)
  let pull u p b =
    match b with
    | "remote" ->
        P.pull [||]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"
    | branch ->
        P.pull [| branch |]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"

  (** [push] pushes files to branch [b]. [u] is the username and [p] is
      the password of the user *)
  let push u p b =
    match b with
    | "remote" ->
        P.push [||]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"
    | branch ->
      P.push [| "origin"; branch |]
        |> P.get_out
        |> List.map rm_leading_spaces
        |> List.rev |> String.concat "\n"

  (**[contains] returns true if string [s1] contains string [s2]*)
  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

  (**[commit_t_of_commit_oneline line] converts [line] to a printable
     output*)
  let commit_t_of_commit_oneline line =
    let hash =
      if contains line "fatal:" then "" else String.sub line 0 7
    in
    let msg =
      try "  " ^ String.sub line 9 (String.length line - 22)
      with Invalid_argument _ -> hash ^ " "
    in
    { tree = hash; msg }

  (** [commit_t_list_of_res res] converts [res] to a list of commits*)
  let commit_t_list_of_res res =
    let lines = P.get_out res in
    List.map commit_t_of_commit_oneline lines
    |> List.filter (fun x -> x.tree <> "")

  (** [log h] is the list of commit objects that are reachable from HEAD
      in reverse chronological order if [h] is [None], otherwise the
      commit objects that are reachable by following parents of commit
      [h] in reverse chronological order *)
  let log hash =
    match hash with
    | None ->
        let res = P.log [| "-10" |] in
        commit_t_list_of_res res
    | Some h ->
        let res = P.log [| h; "-10" |] in
        commit_t_list_of_res res

  (**[branch_msg name] displays the git output message of an operation *)
  let branch_msg name =
    if contains name "fatal:" then ""
    else
      try
        let res = P.log [| "--graph"; name; "-1"; "--format=%s" |] in
        let msg =
          P.get_out res |> List.fold_left (fun acc x -> acc ^ x) ""
        in
        let start = String.index msg '*' in
        String.sub msg (start + 2) (String.length msg - start - 2)
      with Not_found -> ""

  (**[get_head] returns the current head commit*)
  let get_head () =
    let long_ref =
      match P.get_out (P.head [||]) with [] -> "" | h :: t -> h
    in
    let start =
      match long_ref with
      | "" -> 0
      | _ -> (
          try
            Str.search_backward (Str.regexp "heads") long_ref
              (String.length long_ref - 1)
            + 6
          with Not_found -> 0)
    in
    String.sub long_ref start (String.length long_ref - start)

  (**[get_last_msg] returns the first line of the log*)
  let get_last_msg =
    P.get_out (P.log [| "-1"; "--format=%s" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  (**[get_upstream] returns the upstream commit*)
  let get_upstream () =
    P.get_out
      (P.revparse
         [| "--abbrev-ref"; "--symbolic-full-name"; "@{upstream}" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  (**[get_push] returns the push commit*)
  let get_push () =
    P.get_out
      (P.revparse
         [| "--abbrev-ref"; "--symbolic-full-name"; "@{push}" |])
    |> List.fold_left (fun acc x -> acc ^ x) ""

  (** [add fnames] adds the files with filenames [fnames] to the staging
      area *)
  let add files =
    let args_arr = Array.of_list files in
    ignore (P.add args_arr)

  (**[restore_staged files] unstages [files]*)
  let restore_staged files =
    let args_lst = "--staged" :: files in
    let args_arr = Array.of_list args_lst in
    ignore (P.restore args_arr)

  (** [commit msg] commits the changes in the staging area with commit
      message [msg] *)
  let commit msg =
    P.commit [| "-m"; msg |]
    |> P.get_out
    |> List.map rm_leading_spaces
    |> List.rev |> String.concat "\n"

  (** [diff] shows the diffs of tracked files *)
  let diff () =
    P.diff [||]
    |> P.get_out
    |> List.map rm_leading_spaces
    |> List.rev |> String.concat "\n"

  (**[empty_status_t] is a status with no untracked, tracked, or staged
     files *)
  let empty_status_t = { untracked = []; tracked = []; staged = [] }

  (**[add_to_untracked status filename] adds [filename] to the list of
     untracked files and updates [status]*)
  let add_to_untracked status filename =
    {
      tracked = status.tracked;
      untracked = filename :: status.untracked;
      staged = status.staged;
    }

  (**[add_to_tracked status filename] tracks [filename] and updates
     [status]*)
  let add_to_tracked status filename =
    {
      tracked = filename :: status.tracked;
      untracked = status.untracked;
      staged = status.staged;
    }

  (**[add_to_staged status filename] stages [filename] and updates
     [status]*)
  let add_to_staged status filename =
    {
      tracked = status.tracked;
      untracked = status.untracked;
      staged = filename :: status.staged;
    }

  (**[add_to_staged status filename] tracks and stages [filename] and
     updates [status]*)
  let add_to_staged_and_tracked status filename =
    let status' = add_to_staged status filename in
    add_to_tracked status' filename

  (**[add_to_status_t status line] adds [line] to the correct lists
     based on its filename, then updates [status]*)
  let add_to_status_t status line =
    let filename = String.sub line 2 (String.length line - 2) in
    let filename = String.trim filename in
    match String.sub line 0 2 with
    | "??" -> add_to_untracked status filename
    | " M" -> add_to_tracked status filename
    | "M " -> add_to_staged status filename
    | "MM" -> add_to_staged_and_tracked status filename
    | "MD" -> add_to_staged_and_tracked status filename
    | " A" -> add_to_tracked status filename
    | "A " -> add_to_staged status filename
    | "AM" -> add_to_staged_and_tracked status filename
    | "AD" -> add_to_staged_and_tracked status filename
    | " D" -> add_to_tracked status filename
    | "D " -> add_to_staged status filename
    | " R" -> add_to_tracked status filename
    | "R " -> add_to_staged status filename
    | "RM" -> add_to_staged_and_tracked status filename
    | "RD" -> add_to_staged_and_tracked status filename
    | " C" -> add_to_tracked status filename
    | "C " -> add_to_staged status filename
    | "CM" -> add_to_staged_and_tracked status filename
    | "CD" -> add_to_staged_and_tracked status filename
    | "DR" -> add_to_staged_and_tracked status filename
    | "DC" -> add_to_staged_and_tracked status filename
    | "DD" -> add_to_staged_and_tracked status filename
    | "AU" -> add_to_staged_and_tracked status filename
    | "UD" -> add_to_staged_and_tracked status filename
    | "UA" -> add_to_staged_and_tracked status filename
    | "DU" -> add_to_staged_and_tracked status filename
    | "AA" -> add_to_staged_and_tracked status filename
    | "UU" -> add_to_staged_and_tracked status filename
    | _ -> status

  (** [init d] initializes a git repository in the current working
      directory if [n] is [None], otherwise initializes a git repository
      with name [n] as a subdirectory in the current working directory *)
  let init (dir : string option) : unit =
    match dir with
    | None -> ignore (P.init [||])
    | Some d -> ignore (P.init [| d |])

  (**[status_t_of_string_list lines] converts [lines] to a status*)
  let status_t_of_string_list lines =
    List.fold_left add_to_status_t empty_status_t lines

  (**[status] gets the current git status*)
  let status () =
    let status = P.status [| "--porcelain" |] in
    let lines = P.get_out status in
    status_t_of_string_list lines

  (**[checkout branch] switches to branch [branch]*)
  let checkout branch =
    let res = P.checkout [| branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (**[create_branch branch] creates a new branch [branch]*)
  let create_branch branch =
    let res = P.checkout [| "-b"; branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (**[delete_branch branch] deletes branch [branch]*)
  let delete_branch branch =
    let res = P.branch [| "-d"; branch |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (** [reset_hard c] puts the head at the commit with hash [c], deleting
      any changes made after [c]. *)
  let reset_hard commit =
    let res = P.reset [| "--hard"; commit |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (** [reset_soft c] puts the head at the commit with has [c], but keeps
      all changes made after [c]. *)
  let reset_soft commit =
    let res = P.reset [| "--soft"; commit |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (** [stash_apply] applies changes to the current working tree, leaving
      them on the stash stack. *)
  let stash_apply () =
    let res = P.stash [| "apply" |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (** [stash_pop] applies changes to the current working tree and
      removes them from the stash stack. *)
  let stash_pop () =
    let res = P.stash [| "pop" |] in
    P.get_out res |> List.fold_left (fun acc x -> acc ^ x ^ "\n") ""

  (**[get_untracked status] returns a list of all untracked files in
     [status]*)
  let get_untracked status = status.untracked

  (**[get_tracked status] returns a list of all tracked files in
     [status]*)
  let get_tracked status = status.tracked

  (**[get_staged status] returns a list of all staged files in [status]*)
  let get_staged status = status.staged

  (**[string_of_commit_t c] converts the tree and message of [c] to a
     single string separated by a space*)
  let string_of_commit_t c = c.tree ^ " " ^ c.msg
end
