(** Uses functions from all other modules to run commands and display
    the result to the user *)

open Plumbing
open State
open Renderer

(** The plumbing used by OGit while running *)
module MPlumbing = ProdPlumbing

(** The state used by OGit while running *)
module MyState = StateImpl (MPlumbing)

(** The renderer used by OGit while running *)
module MyRenderer = RendererImpl (MyState)

open MPlumbing
open MyState
open MyRenderer

(**[run_input_mode win st] runs OGit in window [win] when [st] is in a
   mode which requires user input*)
let run_input_mode win (st : MyState.t) =
  let input = MyRenderer.render_input_mode st win in
  let cmd =
    match MyState.get_mode st with
    | CommitMode -> Command.Commit input
    | CheckoutGetBranchNameMode -> Command.CheckoutBranch input
    | CreateGetBranchNameMode -> Command.CreateBranch input
    | DeleteGetBranchNameMode -> Command.DeleteBranch input
    | ResetGetCommitMode true -> Command.ResetHard input
    | ResetGetCommitMode false -> Command.ResetSoft input
    | _ -> failwith "Wrong run function"
  in
  MyState.exec st cmd

(**[run_push_mode win st] runs OGit in window [win] when [st] is in
   [Push] mode*)
let run_push_mode win (st : MyState.t) =
  let input = MyRenderer.render_input_mode st win in
  let cmd =
    if input = "" then
      let key = Curses.wgetch win in
      Command.parse_key_push_mode key
    else
      match MyState.get_mode st with
      | PushMode ("", p, b) -> Command.Push (input, p, b)
      | PushMode (u, "", b) -> Command.Push (u, input, b)
      | PushMode (u, p, "") -> Command.Push (u, p, input)
      | PushMode (u, p, b) -> Command.Push (u, p, b)
      | _ -> failwith "Wrong run function"
  in
  let new_st = MyState.update_mode st cmd in
  MyState.exec new_st cmd

(**[run_pull_mode win st] runs OGit in window [win] when [st] is in
   [Pull] mode*)
let run_pull_mode win (st : MyState.t) =
  let input = MyRenderer.render_input_mode st win in
  let cmd =
    if input = "" then
      let key = Curses.wgetch win in
      Command.parse_key_pull_mode key
    else
      match MyState.get_mode st with
      | PullMode ("", p, b) -> Command.Pull (input, p, b)
      | PullMode (u, "", b) -> Command.Pull (u, input, b)
      | PullMode (u, p, "") -> Command.Pull (u, p, input)
      | PullMode (u, p, b) -> Command.Pull (u, p, b)
      | _ -> failwith "Wrong run function"
  in
  let new_st = MyState.update_mode st cmd in
  MyState.exec new_st cmd

(**[run_all_mode win st] runs OGit in window [win] when [st] is in [All]
   mode*)
let run_all_mode win (st : MyState.t) =
  let cmd = Command.StageAll in
  ignore (MyState.exec st cmd);
  let msg = MyRenderer.render_input_mode st win in
  let cmd = Command.Commit msg in
  MyState.exec st cmd

(**[run_normal win st] runs OGit in window [win] when [st] is in any
   mode that requires no user input*)
let run_normal win st parse_fun =
  MyRenderer.render st win;
  let max_y = fst (Curses.getmaxyx win) in
  let key = Curses.wgetch win in
  let cmd = parse_fun key in
  let full_cmd =
    match cmd with
    | Command.NavUp _ ->
        if
          MyState.get_curs st >= max_y - 1
          && not (!MyRenderer.top_line <= 1)
        then Command.NavUp false
        else Command.NavUp true
    | Command.NavDown _ ->
        if MyState.get_curs st >= max_y - 1 then Command.NavDown false
        else Command.NavDown true
    | _ -> cmd
  in
  let new_st = MyState.update_mode st full_cmd in
  MyState.exec new_st full_cmd

(**[run win st] uses the appropriate function to run window [win] based
   on the mode of [st]*)
let rec run win (st : MyState.t) =
  match MyState.get_mode st with
  | MyState.DiffMode _ ->
      run win (run_normal win st Command.parse_key_diff_mode)
  | MyState.CommandDone _ ->
      run win (run_normal win st Command.parse_key)
  | MyState.PushMode (_, _, _) -> run win (run_push_mode win st)
  | MyState.PullMode (_, _, _) -> run win (run_pull_mode win st)
  | MyState.Normal -> run win (run_normal win st Command.parse_key)
  | MyState.BranchMode ->
      run win (run_normal win st Command.parse_key_branch_mode)
  | MyState.NormalTutorialMode ->
      run win (run_normal win st Command.parse_key_normal_tutorial)
  | MyState.DiffTutorialMode ->
      run win (run_normal win st Command.parse_key_diff_tutorial)
  | MyState.PullTutorialMode ->
      run win (run_normal win st Command.parse_key_pull_tutorial)
  | MyState.PushTutorialMode ->
      run win (run_normal win st Command.parse_key_push_tutorial)
  | MyState.BranchTutorialMode ->
      run win (run_normal win st Command.parse_key_branch_tutorial)
  | MyState.ResetTutorialMode ->
      run win (run_normal win st Command.parse_key_reset_tutorial)
  | MyState.StashTutorialMode ->
      run win (run_normal win st Command.parse_key_stash_tutorial)
  | MyState.StashMode ->
      run win (run_normal win st Command.parse_key_stash_mode)
  | MyState.ResetMode ->
      run win (run_normal win st Command.parse_key_reset_mode)
  | MyState.AllMode -> run win (run_all_mode win st)
  | _ -> run win (run_input_mode win st)

(**[run_git args] prints the results of all git operations*)
let run_git args =
  List.iter print_endline (MPlumbing.get_out (MPlumbing.git args))

let () =
  if Array.length Sys.argv > 1 then
    run_git (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  else
    try
      let win = MyRenderer.init () in
      let initial_st = MyState.init_state "." in
      run win initial_st
    with Command.Program_terminate ->
      MyRenderer.cleanup ();
      exit 0
