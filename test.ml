open OUnit2
open Plumbing
open Porcelain
open Renderer
open State

(** Our approach to testing:

    Originally, our project was very difficult to test since every
    module except the Command module depended on side effects in the
    file system.

    For example, if a git stage command was issued, the renderer,
    porcelain, and porcelain module were affected by whether that file
    existed in the filesystem, and whether that file was actually moved
    to the staging area.

    In addition to this, it was difficult to test using the file system
    because our test git files and commands were leaking into the git
    project that was used for tracking the project.

    Lastly, it was difficult because setup and cleanup as hard. We were
    having to write tests that modified the file system, and that made
    it feel like we needed to write tests for our test code. For
    example, we had to write a cleanup function that deleted files, and
    we found that when a test assertion failed, the code to cleanup was
    never executed, leaving our project directory messy and so the tests
    could not be run again without manual cleanup.

    From here, we moved to a different approach: mocking the plumbing
    module, which was the module that interfaced with the file system.
    By doing this, we were able to add methods to it that did setup, so
    when we called normal plumbing methods, we would know exactly what
    the response was going to be.

    This made it easy to test edge cases, stop our tests from affecting
    the git tracking of this project, not have to sory about cleanup,
    and gave us the ability to write tests cases that did what we want
    quickly.

    This means that we were able to test each individual module instead
    of relying on integration tests or testing by manual inspection. At
    this point, we know that each individual module works on its own. We
    know that each individual module is correct.

    Although we did not test the plumbing module explicitly, we were
    able to see that it worked through manual inspection. Since testing
    a GUI is not easy, we relied on using the product ourselves and
    making sure it worked. Through this and our expectations of how git
    performed, we were able to see if we were getting the correct data.
    Since the modules that display data depend on the data returned by
    the plumbing module, its easy to know if the plumbing module was
    working as expected. In addition, the code in the plumbing module is
    quite trivial and its also easy to inspect it and say that if it
    returned information then it worked.

    So although we did not write tests for renderer.ml or plumbing.ml,
    it was implicitly tested through extensive use of the product itself
    as well as the correct data being there when testing the porcelain
    and state modules explicitly. *)

(*****************************************************)
(* Mock Plumbing *)
(*****************************************************)
module TestPorcelain = PorcelainImpl (MockPlumbing)
module TestState = StateImpl (MockPlumbing)

(*****************************************************)
(* Porcelain Tests *)
(*****************************************************)

(** [porcelain_test n f] constructs an OUnit test named [n] that asserts
    no exception is thrown when [f] executed *)
let porcelain_test (name : string) f : test =
  name >:: fun _ ->
  f ();
  assert_bool "no exception thrown" true

let porcelain_tests =
  [
    porcelain_test "pull empty" (fun () -> TestPorcelain.pull "");
    porcelain_test "pull non-empty" (fun () ->
        TestPorcelain.pull "master");
    porcelain_test "push empty" (fun () -> TestPorcelain.push "");
    porcelain_test "push non-empty" (fun () ->
        TestPorcelain.push "master");
    porcelain_test "log none" (fun () -> TestPorcelain.log None);
    porcelain_test "get_head" (fun () -> TestPorcelain.get_head);
    porcelain_test "get_upstream" (fun () -> TestPorcelain.get_upstream);
    porcelain_test "get_push" (fun () -> TestPorcelain.get_push);
    porcelain_test "commit" (fun () -> TestPorcelain.commit "msg");
    porcelain_test "diff" (fun () -> TestPorcelain.diff);
    porcelain_test "status" (fun () -> TestPorcelain.status);
    porcelain_test "checkout" (fun () -> TestPorcelain.checkout "ha");
    porcelain_test "create branch" (fun () ->
        TestPorcelain.create_branch "hah");
    porcelain_test "delete branch" (fun () ->
        TestPorcelain.delete_branch "hah");
  ]

(*****************************************************)
(* State Tests *)
(*****************************************************)

let init_state_test (name : string) setup check : test =
  name >:: fun _ ->
  setup ();
  let a = TestState.init_state "dummy" in
  assert_bool "commit history empty" (check a)

let empty_commit_history () =
  MockPlumbing.set_log_data
    [ "fatal: there is no commit history for this project" ]
    []
    [ "fatal: there is no commit history for this project" ]

let some_commit_history () =
  MockPlumbing.set_log_data
    [
      "59689ce (setup project files, 2021-03-22)";
      "b92c19e (Initial commit, 2021-03-04)";
    ]
    []
    [
      "59689ce (setup project files, 2021-03-22)";
      "b92c19e (Initial commit, 2021-03-04)";
    ]

let is_commit_history_empty st =
  match TestState.commit_history st with [] -> true | h :: t -> false

let is_commit_history_not_empty st =
  match TestState.commit_history st with [] -> false | h :: t -> true

let no_tracked_data () = MockPlumbing.set_status_data [] [] []

let some_tracked_data () =
  MockPlumbing.set_status_data [ " M test.txt" ] [] [ " M test.txt" ]

let is_no_tracked st =
  match TestState.tracked st with [] -> true | h :: t -> false

let is_tracked st =
  match TestState.tracked st with [] -> false | h :: t -> true

let no_staged_data () = MockPlumbing.set_status_data [] [] []

let some_staged_data () =
  MockPlumbing.set_status_data [ "M  test.txt" ] [] [ "M  test.txt" ]

let some_untracked_data () =
  MockPlumbing.set_status_data [ "?? test.txt" ] [] [ "?? test.txt" ]

let is_no_staged st =
  match TestState.staged st with [] -> true | h :: t -> false

let is_staged st =
  match TestState.staged st with [] -> false | h :: t -> true

let is_normal_mode st =
  match TestState.get_mode st with Normal -> true | _ -> false

let set_head () =
  MockPlumbing.set_head_data [ "refs/heads/master" ] []
    [ "refs/heads/master" ]

let head_exists st =
  match TestState.head st with "" -> false | _ -> true

let init_state_tests =
  [
    init_state_test "no commit history" empty_commit_history
      is_commit_history_empty;
    init_state_test "some commit history" some_commit_history
      is_commit_history_not_empty;
    init_state_test "no tracked" no_tracked_data is_no_tracked;
    init_state_test "some tracked" some_tracked_data is_tracked;
    init_state_test "no staged" no_staged_data is_no_staged;
    init_state_test "some staged" some_staged_data is_staged;
    init_state_test "is in normal mode" (fun () -> ()) is_normal_mode;
    init_state_test "head is populated" set_head head_exists;
  ]

let is_curs c st = TestState.get_curs st = c

let exec_test (name : string) cmd setup effect check : test =
  name >:: fun _ ->
  setup ();
  let st = TestState.init_state "as" in
  let st' = TestState.exec st cmd in
  (*effect ();*)
  assert_bool "exec check" (check st')

let exec_tests =
  [
    exec_test "stage staged file" Command.Stage some_staged_data
      some_staged_data is_staged;
    exec_test "stage untracked file" Command.Stage some_untracked_data
      some_staged_data is_no_staged;
    exec_test "stage tracked file" Command.Stage some_tracked_data
      some_staged_data is_no_staged;
    exec_test "unstage file" Command.Unstage some_staged_data
      some_staged_data is_staged;
    exec_test "navup at top of file" (Command.NavUp true)
      (fun () -> ())
      (fun () -> ())
      (is_curs 0);
    exec_test "navdown top of file" (Command.NavDown true)
      (fun () -> ())
      (fun () -> ())
      (is_curs 1);
  ]

let printable_of_state_tests = []

let set_mode_test (name : string) mode : test =
  name >:: fun _ ->
  let st = TestState.init_state "as" in
  let st' = TestState.set_mode st mode in
  assert_bool "set mode" (TestState.get_mode st' = mode)

let set_mode_tests =
  [
    set_mode_test "normal" TestState.Normal;
    set_mode_test "commit mode" TestState.CommitMode;
    set_mode_test "commit done" (TestState.CommandDone "");
    set_mode_test "diff" (TestState.DiffMode "");
    set_mode_test "push" (TestState.PushMode ("", "", ""));
    set_mode_test "pull" (TestState.PullMode ("", "", ""));
    set_mode_test "branch" TestState.BranchMode;
    set_mode_test "checkout branch" TestState.CheckoutGetBranchNameMode;
    set_mode_test "create branch" TestState.CreateGetBranchNameMode;
    set_mode_test "delete branch" TestState.DeleteGetBranchNameMode;
    set_mode_test "pull elsewhere" (TestState.PullMode ("a", "b", "c"));
    set_mode_test "push elsewhere" (TestState.PushMode ("a", "b", "c"));
    set_mode_test "stash" TestState.StashMode;
    set_mode_test "reset" TestState.ResetMode;
    set_mode_test "reset hard get commit"
      (TestState.ResetGetCommitMode true);
    set_mode_test "reset soft get commit"
      (TestState.ResetGetCommitMode false);
  ]

let state_tests = init_state_tests @ exec_tests @ set_mode_tests

(*****************************************************)
(* Command Tests *)
(*****************************************************)

(** [parse_key_test k exp] constructs an OUnit test named [n] that
    asserts [Command.parse_key k] is exp*)
let parse_key_test (name : string) (key : int) (exp : string) : test =
  name >:: fun _ ->
  assert_equal exp (Command.string_of_cmd (Command.parse_key key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_diff_mode k] is exp*)
let parse_key_diff_mode_test (name : string) (key : int) (exp : string)
    : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_diff_mode key))

(** [parse_key_diff_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_pull_mode k] is exp*)
let parse_key_pull_mode_test (name : string) (key : int) (exp : string)
    : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_pull_mode key))

(** [parse_key_push_mode_test k exp] constructs an OUnit test named [n]
    that asserts [Command.parse_key_push_mode k] is exp*)
let parse_key_push_mode_test (name : string) (key : int) (exp : string)
    : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_push_mode key))

(** [parse_key_branch_mode_test k exp] constructs an OUnit test named
    [n] that asserts [Command.parse_key_branch_mode k] is exp*)
let parse_key_branch_mode_test
    (name : string)
    (key : int)
    (exp : string) : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_branch_mode key))

let parse_key_stash_mode_test (name : string) (key : int) (exp : string)
    : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_stash_mode key))

let parse_key_reset_mode_test (name : string) (key : int) (exp : string)
    : test =
  name >:: fun _ ->
  assert_equal exp
    (Command.string_of_cmd (Command.parse_key_reset_mode key))

(** Tests for [Command.parse_key] *)
let parse_key_tests =
  [
    parse_key_test "s is stage" (int_of_char 's') "stage";
    parse_key_test "u is unstage" (int_of_char 'u') "unstage";
    parse_key_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_test "Down is NavDown" Curses.Key.down "navdown";
    parse_key_test "q is quit" (int_of_char 'q') "quit";
    parse_key_test "unsupported is nop" (int_of_char '[') "nop";
    parse_key_test "space is clear" (int_of_char ' ') "clear";
  ]

let parse_key_diff_mode_tests =
  [
    parse_key_diff_mode_test "s is diff" (int_of_char 's') "diff";
    parse_key_diff_mode_test "t is diff" (int_of_char 't') "diff";
    parse_key_diff_mode_test "a is diff" (int_of_char 'a') "diff";
    parse_key_diff_mode_test "f is diff" (int_of_char 'f') "diff";
    parse_key_diff_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_diff_mode_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_diff_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_diff_mode_test "Down is NavDown" Curses.Key.down "navdown";
    parse_key_diff_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_diff_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_diff_mode_test "space is clear" (int_of_char ' ') "clear";
  ]

let parse_key_pull_mode_tests =
  [
    parse_key_pull_mode_test "p is pull" (int_of_char 'p') "pull";
    parse_key_pull_mode_test "u is pull" (int_of_char 'u') "pull";
    parse_key_pull_mode_test "e is pull" (int_of_char 'e') "pull";
    parse_key_pull_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_pull_mode_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_pull_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_pull_mode_test "Down is NavDown" Curses.Key.down "navdown";
    parse_key_pull_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_pull_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_pull_mode_test "space is clear" (int_of_char ' ') "clear";
  ]

let parse_key_push_mode_tests =
  [
    parse_key_push_mode_test "p is push" (int_of_char 'p') "push";
    parse_key_push_mode_test "u is push" (int_of_char 'u') "push";
    parse_key_push_mode_test "e is push" (int_of_char 'e') "push";
    parse_key_push_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_push_mode_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_push_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_push_mode_test "Down is NavDown" Curses.Key.down "navdown";
    parse_key_push_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_push_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_push_mode_test "space is clear" (int_of_char ' ') "clear";
  ]

let parse_key_branch_mode_tests =
  [
    parse_key_branch_mode_test "b is branch" (int_of_char 'b')
      "checkout branch prompt";
    parse_key_branch_mode_test "c is branch" (int_of_char 'c')
      "create branch prompt";
    parse_key_branch_mode_test "x is branch" (int_of_char 'x')
      "delete branch prompt";
    parse_key_branch_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_branch_mode_test "j is NavDown" (int_of_char 'j')
      "navdown";
    parse_key_branch_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_branch_mode_test "Down is NavDown" Curses.Key.down
      "navdown";
    parse_key_branch_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_branch_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_branch_mode_test "space is clear" (int_of_char ' ')
      "clear";
  ]

let parse_key_stash_mode_tests =
  [
    parse_key_stash_mode_test "a is stash" (int_of_char 'a') "stash";
    parse_key_stash_mode_test "p is stash" (int_of_char 'p') "stash";
    parse_key_stash_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_stash_mode_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_stash_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_stash_mode_test "Down is NavDown" Curses.Key.down
      "navdown";
    parse_key_stash_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_stash_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_stash_mode_test "space is clear" (int_of_char ' ') "clear";
  ]

let parse_key_reset_mode_tests =
  [
    parse_key_reset_mode_test "h is reset" (int_of_char 'h') "reset";
    parse_key_reset_mode_test "s is reset" (int_of_char 's') "reset";
    parse_key_reset_mode_test "k is NavUp" (int_of_char 'k') "navup";
    parse_key_reset_mode_test "j is NavDown" (int_of_char 'j') "navdown";
    parse_key_reset_mode_test "Up is NavUp" Curses.Key.up "navup";
    parse_key_reset_mode_test "Down is NavDown" Curses.Key.down
      "navdown";
    parse_key_reset_mode_test "q is quit" (int_of_char 'q') "quit";
    parse_key_reset_mode_test "unsupported is nop" (int_of_char '[')
      "nop";
    parse_key_reset_mode_test "space is clear" (int_of_char ' ') "clear";
  ]

(** Tests for [Command] module *)
let command_tests =
  parse_key_tests @ parse_key_diff_mode_tests
  @ parse_key_pull_mode_tests @ parse_key_push_mode_tests
  @ parse_key_branch_mode_tests

(*****************************************************)
(* Test Suite *)
(*****************************************************)

let suite =
  "test suite for ogit"
  >::: List.flatten [ command_tests; state_tests; porcelain_tests ]

let _ = run_test_tt_main suite
