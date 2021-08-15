(** Representation the git repository and user interface *)

open Plumbing
open Porcelain

module type State = sig
  (** The porcelain used by an abstract [State] to execute commands *)
  module MPorcelain : Porcelain

  (** The abstract type of values representing the git state *)
  type t

  (** The variant type representing the differences in the VIM when
      various commands are called *)
  type render_mode =
    | Normal
    | CommitMode
    | CommandDone of string
    | DiffMode of string
    | PushMode of string * string * string
    | PullMode of string * string * string
    | BranchMode
    | CheckoutGetBranchNameMode
    | CreateGetBranchNameMode
    | DeleteGetBranchNameMode
    | PullElsewhereMode
    | NormalTutorialMode
    | DiffTutorialMode
    | PullTutorialMode
    | PushTutorialMode
    | BranchTutorialMode
    | StashTutorialMode
    | ResetTutorialMode
    | StashMode
    | AllMode
    | ResetMode
    | ResetGetCommitMode of bool

  (** The variant type representing whether or not the cursor is
      attempting to go off the screen, and if it is, then whether it is
      attempting to go up or down*)
  type curs_state =
    | OffScrUp
    | OffScrDown
    | OnScr

  (** The representation type that specifies what [color] [text] should
      be printed as. *)
  type printable = {
    text : string;
    color : string;
  }

  (** [init_state dir] is the state of the current working tree
      belonging to the repo rooted at [dir]. *)
  val init_state : string -> t

  (** [commit_history st] is the commit history of the current state
      [st] *)
  val commit_history : t -> MPorcelain.commit_t list

  (** [tracked st] is the list of all tracked files in [st] *)
  val tracked : t -> string list

  (** [staged st] is the list of all staged files in [st] *)
  val staged : t -> string list

  (** [head st] is the commit pointed to by the head in the current
      state [st] *)
  val head : t -> string

  (** [merge st] is the commit pointed to by the upstream branch of the
      current branch *)
  val merge : t -> string

  (** [push st] is the branch that the current branch is being pushed to *)
  val push : t -> string

  (** [exec st c] is the state after executing command [c] from state
      [st]. *)
  val exec : t -> Command.t -> t

  (** [printable_of_state st] is a printable represnation of the state *)
  val printable_of_state : t -> printable list

  (** [get_curs st] is the y index of the position into state. *)
  val get_curs : t -> int

  (** [set_curs st y b] is the state whose cursor is [y], with [b]
      documenting whether or not the cursor is attempting to go
      offscreen. The rest of the state says the same. *)
  val set_curs : t -> int -> curs_state -> t

  (** [get_mode st] is the rendering mode for [st]. *)
  val get_mode : t -> render_mode

  (** [set_mode st new_mode] manually changes the mode of [st] to
      [new_mode]. *)
  val set_mode : t -> render_mode -> t

  (** [get_curs_state st] returns whether or not the cursor of [st] is
      attempting to leave the screen, and if it is, then whether it is
      attempting to go up or down *)
  val get_curs_state : t -> curs_state

  (** [update_mode st cmd] automatically changes the mode of [st] to the
      appropriate mode based on the command [cmd]. The rest of the state
      stays the same. *)
  val update_mode : t -> Command.t -> t

  (** [curs_at_commit st] returns true if the cursor is hovering over a
      commit hash in [st]. *)
  val curs_at_commit : t -> bool
end

(** The concrete [State] used by OGit to track effects of various
    commands *)
module StateImpl (P : Plumbing) : State = struct
  (** The porcelain used by StateImpl to run commands *)
  module MPorcelain = PorcelainImpl (P)

  (** The variant type in [StateImpl] representing the differences in
      the VIM when various commands are called *)
  type render_mode =
    | Normal
    | CommitMode
    | CommandDone of string
    | DiffMode of string
    | PushMode of string * string * string
    | PullMode of string * string * string
    | BranchMode
    | CheckoutGetBranchNameMode
    | CreateGetBranchNameMode
    | DeleteGetBranchNameMode
    | PullElsewhereMode
    | NormalTutorialMode
    | DiffTutorialMode
    | PullTutorialMode
    | PushTutorialMode
    | BranchTutorialMode
    | StashTutorialMode
    | ResetTutorialMode
    | StashMode
    | AllMode
    | ResetMode
    | ResetGetCommitMode of bool

  (** The variant type in [StateImpl] representing whether or not the
      cursor is attempting to go off the screen, and if it is, then
      whether it is attempting to go up or down*)
  type curs_state =
    | OffScrUp
    | OffScrDown
    | OnScr

  (** The representation type for state in [StateImpl]. *)
  type t = {
    commit_history : MPorcelain.commit_t list;
    head : string;
    merge : string;
    push : string;
    untracked : string list;
    tracked : string list;
    staged : string list;
    curs : int;
    mode : render_mode;
    curs_st : curs_state;
  }

  (** The representation type in [StateImpl] that specifies what [color]
      [text] should be printed as. *)
  type printable = {
    text : string;
    color : string;
  }

  (** [init_state dir] is the state of the directory [dir]. The cursor
      points to the first line of the terminal. Requires [dir] is a
      directory containing a valid .git directory *)
  let init_state dir =
    {
      commit_history = MPorcelain.log None;
      head = MPorcelain.get_head ();
      merge = MPorcelain.get_upstream ();
      push = MPorcelain.get_push ();
      untracked = MPorcelain.get_untracked (MPorcelain.status ());
      tracked = MPorcelain.get_tracked (MPorcelain.status ());
      staged = MPorcelain.get_staged (MPorcelain.status ());
      curs = 0;
      mode = Normal;
      curs_st = OnScr;
    }

  (** [update_git_state st] updates commit_history, untracked, tracked
      and staged files according to the git directory *)
  let update_git_state st =
    {
      commit_history = MPorcelain.log None;
      head = MPorcelain.get_head ();
      merge = MPorcelain.get_upstream ();
      push = MPorcelain.get_push ();
      untracked = MPorcelain.get_untracked (MPorcelain.status ());
      tracked = MPorcelain.get_tracked (MPorcelain.status ());
      staged = MPorcelain.get_staged (MPorcelain.status ());
      curs = st.curs;
      mode = st.mode;
      curs_st = st.curs_st;
    }

  (*********************************************************)
  (* Access/Mutate state *)
  (*********************************************************)

  (** [head st] is the commit pointed to by the head in the current
      state [st] *)
  let head st = st.head

  (** [merge st] is the commit pointed to by the upstream branch of the
      current branch *)
  let merge st = st.merge

  (** [push st] is the branch that the current branch is being pushed to *)
  let push st = st.push

  (** [untracked st] is the list of all untracked files in [st] *)
  let untracked st = st.untracked

  (** [tracked st] is the list of all tracked files in [st] *)
  let tracked st = st.tracked

  (**[staged st] is the list of all staged files in [st]*)
  let staged st = st.staged

  (**[commit_history st] is the list of all past commits in [st] in
     chronological order*)
  let commit_history st = st.commit_history

  (**[get_curs st] is the position of the cursor in [st], with the top
     of the screen being 0*)
  let get_curs st = st.curs

  (**[get_mode st] is the mode under which [st] is operating. It can be
     any of the modes in type [render_mode].*)
  let get_mode st = st.mode

  (**[max_curs_pos_normal st] is the maximum possible cursor position
     assuming [st] is in normal mode*)
  let max_curs_pos_normal st =
    List.length st.untracked
    + List.length st.tracked + List.length st.staged + 21

  (**[set_curs st i] moves the cursor in [st] to position [i]*)
  let set_curs st i curs_st =
    let y =
      match curs_st with
      | OffScrUp -> st.curs
      | OffScrDown -> st.curs
      | OnScr -> if i >= 0 then i else 0
    in
    {
      commit_history = st.commit_history;
      head = st.head;
      merge = st.merge;
      push = st.push;
      untracked = st.untracked;
      tracked = st.tracked;
      staged = st.staged;
      curs = y;
      mode = st.mode;
      curs_st;
    }

  (**[set_mode st new_mode] sets the render mode of [st] to [new_mode]*)
  let set_mode st new_mode =
    {
      commit_history = st.commit_history;
      head = st.head;
      push = st.push;
      merge = st.merge;
      untracked = st.untracked;
      tracked = st.tracked;
      staged = st.staged;
      curs = st.curs;
      mode = new_mode;
      curs_st = st.curs_st;
    }

  (** [update_mode st cmd] automatically changes the mode of [st] to the
      appropriate mode based on the command [cmd]. The rest of the state
      stays the same. *)
  let update_mode st cmd =
    let new_mode =
      match cmd with
      | Command.Commit _ -> CommitMode
      | Command.Pull ("m", "m", "m") -> PullMode ("", "", "")
      | Command.Push ("m", "m", "m") -> PushMode ("", "", "")
      | Command.Push (u, p, b) -> PushMode (u, p, b)
      | Command.Pull (u, p, b) -> PullMode (u, p, b)
      | _ -> st.mode
    in
    set_mode st new_mode

  (** [get_curs_state st] returns whether or not the cursor of [st] is
      attempting to leave the screen, and if it is, then whether it is
      attempting to go up or down *)
  let get_curs_state st = st.curs_st

  (*********************************************************)
  (* Printable *)
  (*********************************************************)

  (** [printable_of_file c f] returns the filename of [f] as a printable
      with color [c]*)
  let printable_of_file c f = { text = f; color = c }

  (**[commit_header] labels the commit history *)
  let commit_header = { text = "Recent Commits"; color = "yellow" }

  (**[head_header] labels the head commit*)
  let head_header = { text = "Head"; color = "yellow" }

  (**[merge_header] labels the merge commit*)
  let merge_header = { text = "Merge"; color = "yellow" }

  (**[push_header] labels the push commit*)
  let push_header = { text = "Push"; color = "yellow" }

  (**[untracked_header] labels the list of untracked files*)
  let untracked_header = { text = "Untracked"; color = "yellow" }

  (**[tracked_header] labels the list of tracked files*)
  let tracked_header = { text = "Tracked"; color = "yellow" }

  (**[staged_header] labels the list of staged files*)
  let staged_header = { text = "Staged"; color = "yellow" }

  (**[help] displays the instructions for toggling help*)
  let help = { text = "(To toggle help, press \'i\'.)"; color = "blue" }

  (**[blank_line] is a line with no text in it*)
  let blank_line = { text = " "; color = "white" }

  (**[printable_of_commit_t c] provides a printable representation for
     commit [c]*)
  let printable_of_commit_t c =
    { text = MPorcelain.string_of_commit_t c; color = "white" }

  (**[printable_of_state st] provides a printable representation of [st]*)
  let printable_of_state st =
    let commits_printable =
      List.map printable_of_commit_t (commit_history st) |> List.rev
    in
    let head_printable =
      {
        text = head st ^ "   " ^ MPorcelain.get_last_msg;
        color = "white";
      }
    in
    let merge_printable =
      {
        text = merge st ^ "   " ^ MPorcelain.branch_msg (merge st);
        color = "white";
      }
    in
    let push_printable =
      {
        text = push st ^ "   " ^ MPorcelain.branch_msg (push st);
        color = "white";
      }
    in
    let untracked_printable =
      List.map (printable_of_file "red") (untracked st)
    in
    let tracked_printable =
      List.map (printable_of_file "red") (tracked st)
    in
    let staged_printable =
      List.map (printable_of_file "green") (staged st)
    in
    untracked_header :: untracked_printable
    @ tracked_header :: tracked_printable
    @ staged_header :: staged_printable
    @ [ blank_line ]
    @ [ head_header; head_printable ]
    @ [ merge_header; merge_printable ]
    @ [ push_header; push_printable ]
    @ [ blank_line ]
    @ commit_header :: commits_printable
    @ [ blank_line ] @ [ help ]

  (*********************************************************)
  (* Exec *)
  (*********************************************************)

  (**[get_curs_content st] returns the name of the file over which the
     cursor is hovering in [st]*)
  let get_curs_content st =
    let printables = printable_of_state st in
    let printable = List.nth printables st.curs in
    printable.text

  (**[exec_clear st] executes the [Clear] command and updates [st] *)
  let exec_clear st =
    let new_mode_st = set_mode st Normal in
    let new_curs =
      if st.curs >= max_curs_pos_normal st then max_curs_pos_normal st
      else st.curs
    in
    set_curs new_mode_st new_curs OnScr

  (**[exec_add st] executes the [Stage] command on the file over which
     the cursor is hovering and updates [st] *)
  let exec_add st =
    let curs_content = get_curs_content st in
    MPorcelain.add [ curs_content ];
    update_git_state st

  (**[exec_unstage st] executes the [Unstage] command on the files over
     which the cursor is hovering and updates [st] *)
  let exec_unstage st =
    let curs_content = get_curs_content st in
    MPorcelain.restore_staged [ curs_content ];
    update_git_state st

  (** [exec_commit st msg] executes the [Commit] command with message
      [msg] and updates [st]*)
  let exec_commit st msg =
    let output = MPorcelain.commit msg in
    set_mode (update_git_state st) (CommandDone output)

  (**[exec_diff_tracked st] shows the diff of all tracked files in [st]*)
  let exec_diff_tracked st =
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.untracked;
    MPorcelain.restore_staged st.tracked;
    set_mode st (DiffMode out)

  (*[exec_diff_staged st] shows the diff of all staged files in [st]*)
  let exec_diff_staged st =
    MPorcelain.add st.tracked;
    MPorcelain.restore_staged st.staged;
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.tracked;
    MPorcelain.add st.staged;
    set_mode st (DiffMode out)

  (**[exec_diff_all st] shows the diff of all files in [st]*)
  let exec_diff_all st =
    MPorcelain.restore_staged st.staged;
    let out = MPorcelain.diff () in
    MPorcelain.add st.staged;
    set_mode st (DiffMode out)

  (**[exec_diff_file st] shows the diff of the file over which the
     cursor is hovering in [st]*)
  let exec_diff_file st =
    MPorcelain.add st.tracked;
    let curs_content = get_curs_content st in
    MPorcelain.restore_staged [ curs_content ];
    let out = MPorcelain.diff () in
    MPorcelain.restore_staged st.tracked;
    if List.mem curs_content st.staged then
      MPorcelain.add [ curs_content ]
    else ();
    set_mode st (DiffMode out)

  (**[curs_at_commit st] returns true if the cursor of [st] is hovering
     over a commit*)
  let curs_at_commit st =
    get_curs st <= max_curs_pos_normal st
    && get_curs st >= max_curs_pos_normal st - 9

  (**[exec_pull st u p b] executes the [Pull] command on branch [b] with
     username [u] and password [p] and updates [st]*)
  let exec_pull st u p b =
    if u = "" || p = "" || b = "" then set_mode st (PullMode (u, p, b))
    else if u = "m" && p = "m" && b = "m" then
      set_mode st (PullMode ("", "", ""))
    else
      let out = MPorcelain.pull u p b in
      set_mode st (CommandDone out)

  (**[exec_reset st commit hard] executes the [ResetHard] command if
     [hard] is [true] or the [ResetSoft] command if [hard] is [false],
     then updates [st]*)
  let exec_reset st commit hard =
    if commit = "" && not (curs_at_commit st) then
      set_mode st (ResetGetCommitMode hard)
    else
      let cmt =
        if commit = "" && curs_at_commit st then
          String.sub (get_curs_content st) 0 7
        else commit
      in
      let out =
        if hard then MPorcelain.reset_hard cmt
        else MPorcelain.reset_soft cmt
      in
      set_mode (update_git_state st) (CommandDone out)

  (**[exec_push st u p b] executes the [Push] command on branch [b] with
     username [u] and password [p] and updates [st]*)
  let exec_push st u p b =
    if u = "" || p = "" || b = "" then set_mode st (PullMode (u, p, b))
    else if u = "m" && p = "m" && b = "m" then
      set_mode st (PullMode ("", "", ""))
    else
      let out = MPorcelain.push u p b in
      set_mode st (CommandDone out)

  (**[exec_checkout_branch st branch] executes the [Checkout] command on
     branch [branch] and updates [st]*)
  let exec_checkout_branch st branch =
    let msg = MPorcelain.checkout branch in
    set_mode (update_git_state st) (CommandDone msg)

  (**[exec_create_branch st branch] executes the [CreateBranch] command
     on branch [branch] and updates [st]*)
  let exec_create_branch st branch =
    let msg = MPorcelain.create_branch branch in
    set_mode (update_git_state st) (CommandDone msg)

  (**[exec_delete_branch st branch] executes the [DeleteBranch] command
     on branch [branch] and updates [st]*)
  let exec_delete_branch st branch =
    let msg = MPorcelain.delete_branch branch in
    set_mode (update_git_state st) (CommandDone msg)

  (**[exec_stash_apply st] executes the [StashApply] command and updates
     [st]*)
  let exec_stash_apply st =
    let out = MPorcelain.stash_apply () in
    set_mode (update_git_state st) (CommandDone out)

  (**[exec_stash_pop st] executes the [StashPop] command and updates
     [st]*)
  let exec_stash_pop st =
    let out = MPorcelain.stash_pop () in
    set_mode (update_git_state st) (CommandDone out)

  (**[exec_stage_all st] executes the [StageAll] command and updates
     [st]*)
  let exec_stage_all st =
    MPorcelain.add st.untracked;
    ignore (update_git_state st);
    MPorcelain.add st.tracked;
    update_git_state st

  (**[exec_unstage_all st] executes the [UnstageAll] command and updates
     [st]*)
  let exec_unstage_all st =
    MPorcelain.restore_staged st.staged;
    update_git_state st

  (**[pos_of_cmd] converts a navigation command to a cursor state*)
  let pos_of_cmd = function
    | Command.NavDown true -> OnScr
    | Command.NavDown false -> OffScrDown
    | Command.NavUp true -> OnScr
    | Command.NavUp false -> OffScrUp
    | _ -> failwith "Type error"

  (**[exec st cmd] executes [cmd] and updates [st]*)
  let exec st cmd =
    match cmd with
    (* META COMMANDS *)
    | Command.NavUp b -> set_curs st (get_curs st - 1) (pos_of_cmd cmd)
    | Command.NavDown b ->
        set_curs st (get_curs st + 1) (pos_of_cmd cmd)
    | Command.DiffMenu -> set_mode st (DiffMode "MENU")
    | Command.Clear -> exec_clear st
    | Command.PullMenu -> set_mode st (PullMode ("m", "m", "m"))
    | Command.PushMenu -> set_mode st (PushMode ("m", "m", "m"))
    | Command.BranchMenu -> set_mode st BranchMode
    | Command.All -> set_mode st AllMode
    | Command.Nop -> st
    | Command.Quit -> raise Command.Program_terminate
    (* TUTORIAL *)
    | Command.NormalTutorial -> set_mode st NormalTutorialMode
    | Command.DiffTutorial -> set_mode st DiffTutorialMode
    | Command.PullTutorial -> set_mode st PullTutorialMode
    | Command.PushTutorial -> set_mode st PushTutorialMode
    | Command.BranchTutorial -> set_mode st BranchTutorialMode
    | Command.StashTutorial -> set_mode st StashTutorialMode
    | Command.ResetTutorial -> set_mode st ResetTutorialMode
    | Command.BackNormal -> set_mode st Normal
    | Command.BackDiff -> set_mode st (DiffMode "MENU")
    | Command.BackPull -> set_mode st (PullMode ("m", "m", "m"))
    | Command.BackPush -> set_mode st (PushMode ("m", "m", "m"))
    | Command.BackBranch -> set_mode st BranchMode
    | Command.BackStash -> set_mode st StashMode
    | Command.BackReset -> set_mode st ResetMode
    (* NORMAL MODE *)
    | Command.Stage -> exec_add st
    | Command.StageAll -> exec_stage_all st
    | Command.UnstageAll -> exec_unstage_all st
    | Command.Unstage -> exec_unstage st
    | Command.Commit msg -> if msg = "" then st else exec_commit st msg
    (* DIFF MODE *)
    | Command.DiffTracked -> exec_diff_tracked st
    | Command.DiffStaged -> exec_diff_staged st
    | Command.DiffAll -> exec_diff_all st
    | Command.DiffFile -> exec_diff_file st
    (* PULL MODE *)
    | Command.Pull (u, p, b) ->
        if u = "" || p = "" || b = "" then st else exec_pull st u p b
    (* PUSH MODE *)
    | Command.Push (u, p, b) ->
        if u = "" || p = "" || b = "" then st else exec_push st u p b
    (* BRANCH MODE *)
    | Command.CheckoutBranch b ->
        if b = "" then st else exec_checkout_branch st b
    | Command.CreateBranch b ->
        if b = "" then st else exec_create_branch st b
    | Command.DeleteBranch b ->
        if b = "" then st else exec_delete_branch st b
    | Command.CheckoutBranchPrompt ->
        set_mode st CheckoutGetBranchNameMode
    | Command.CreateBranchPrompt -> set_mode st CreateGetBranchNameMode
    | Command.DeleteBranchPrompt -> set_mode st DeleteGetBranchNameMode
    (* STASH MODE *)
    | Command.Stash -> set_mode st StashMode
    | Command.StashApply -> exec_stash_apply st
    | Command.StashPop -> exec_stash_pop st
    (* RESET MODE *)
    | Command.ResetMenu -> set_mode st ResetMode
    | Command.ResetHard c -> exec_reset st c true
    | Command.ResetSoft c -> exec_reset st c false
end
