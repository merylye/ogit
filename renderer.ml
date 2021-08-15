(** Render state to the terminal *)

open Curses
open State

module type Renderer = sig
  (** The abstract module of type [State] uesd by any module of type
      [Renderer] to access information about the git state *)
  module MState : State

  (** [init ()] is a curses window. Its side effects include
      initialization of colors, enabling cbreak, enabling noecho,
      disables the curser, and clearning the window *)
  val init : unit -> Curses.window

  (** [top_line] is the number of the line currently at the top of the
      screen. Initially, [top_line] is 0, but it can increase or
      decrease as a user scrolls up and down. *)
  val top_line : int ref

  (** [cleanup ()] ends the window and cleans up the side effects
      created by [init ()]*)
  val cleanup : unit -> unit

  (** [render state win] prints all necessary information from [state]
      to window [win]. *)
  val render : MState.t -> Curses.window -> unit

  (** [render_input_mode state win] prints all necessary information
      from [state] to window [win] and retrieves input from the user. *)
  val render_input_mode : MState.t -> Curses.window -> string

  (** [get_color str] maps [str], the name of a color, to the integer
      used by Curses to represent that color. *)
  val get_color : string -> int
end

(** The concrete [Renderer] used by OGit to display information to the
    user *)
module RendererImpl (St : State) : Renderer with module MState = St =
struct
  (** The [State] module used by [RendererImpl] to access information
      about the git state *)
  module MState = St

  let check_err err =
    if err = true then () else raise Command.Program_terminate

  let init_colors () =
    check_err (Curses.start_color ());
    check_err (Curses.init_pair 1 Curses.Color.red Curses.Color.black);
    check_err (Curses.init_pair 2 Curses.Color.green Curses.Color.black);
    check_err
      (Curses.init_pair 3 Curses.Color.yellow Curses.Color.black);
    check_err (Curses.init_pair 4 Curses.Color.blue Curses.Color.black);
    check_err
      (Curses.init_pair 5 Curses.Color.magenta Curses.Color.black);
    check_err (Curses.init_pair 6 Curses.Color.cyan Curses.Color.black);
    check_err (Curses.init_pair 7 Curses.Color.white Curses.Color.black);
    check_err (Curses.init_pair 8 Curses.Color.black Curses.Color.red);
    check_err (Curses.init_pair 9 Curses.Color.black Curses.Color.green);
    check_err
      (Curses.init_pair 10 Curses.Color.black Curses.Color.yellow);
    check_err (Curses.init_pair 11 Curses.Color.black Curses.Color.blue);
    check_err
      (Curses.init_pair 12 Curses.Color.black Curses.Color.magenta);
    check_err (Curses.init_pair 13 Curses.Color.black Curses.Color.cyan);
    check_err
      (Curses.init_pair 14 Curses.Color.black Curses.Color.white)

  (** [get_color str] maps [str], the name of a color, to the integer
      used by Curses to represent that color. *)
  let get_color color =
    let colors =
      [
        "red";
        "green";
        "yellow";
        "blue";
        "magenta";
        "cyan";
        "white";
        "red_back";
        "green_back";
        "yellow_back";
        "blue_back";
        "magenta_back";
        "cyan_back";
        "white_back";
      ]
    in
    let rec r_color color n =
      if List.nth colors n = color then n + 1 else r_color color (n + 1)
    in
    r_color color 0

  let screen = ref [||]

  (** [top_line] is the number of the line currently at the top of the
      screen. Initially, [top_line] is 0, but it can increase or
      decrease as a user scrolls up and down. *)
  let top_line = ref 0

  let enable_color color =
    Curses.attron (Curses.A.color_pair (get_color color))

  let disable_color color =
    Curses.attroff (Curses.A.color_pair (get_color color))

  (**[init] sets up preferences for the OGit window *)
  let init () : Curses.window =
    let win = Curses.initscr () in
    init_colors ();
    check_err (Curses.cbreak ());
    check_err (Curses.noecho ());
    check_err (Curses.curs_set 0);
    check_err (Curses.keypad win true);
    Curses.clear ();
    win

  (**[cleanup] performs all required operations to quit OGit*)
  let cleanup () =
    check_err (Curses.curs_set 1);
    check_err (Curses.echo ());
    check_err (Curses.nocbreak ());
    Curses.endwin ()

  let cursor_nextline win =
    let yx = Curses.getyx win in
    let new_y =
      if fst yx < fst (Curses.getmaxyx win) then fst yx + 1 else fst yx
    in
    let new_yx = (new_y, 0) in
    check_err (Curses.wmove win (fst new_yx) (snd new_yx))

  let cursor_prevline win =
    let yx = Curses.getyx win in
    let new_yx = (fst yx - 1, 0) in
    check_err (Curses.wmove win (fst new_yx) (snd new_yx))

  let cursor_reset win = check_err (Curses.wmove win 0 0)

  let rec render_user_line win (line : MState.printable) =
    enable_color "cyan_back";
    screen := Array.append !screen [| line |];
    let fst_char =
      if String.length line.text = 0 then " "
      else String.sub line.text 0 1
    in
    let rest =
      if String.length line.text <= 1 then ""
      else String.sub line.text 1 (String.length line.text - 1)
    in
    check_err (Curses.waddstr win fst_char);
    disable_color "cyan_back";
    enable_color line.color;
    check_err (Curses.waddstr win rest);
    disable_color line.color;
    cursor_nextline win

  let render_line win curs render_curs (line : MState.printable) =
    let yx = Curses.getyx win in
    screen := Array.append !screen [| line |];
    if fst yx >= fst (Curses.getmaxyx win) - 1 then ()
    else if fst yx = curs && render_curs then render_user_line win line
    else (
      enable_color line.color;
      check_err (Curses.waddstr win line.text);
      disable_color line.color;
      cursor_nextline win )

  let render_lines win lines curs render_curs =
    List.iter (render_line win curs render_curs) lines

  let rec parse_string win str echo =
    if echo then check_err (Curses.echo ());
    enable_color "cyan_back";
    check_err (Curses.waddstr win " ");
    disable_color "cyan_back";
    let yx = Curses.getyx win in
    check_err (Curses.wmove win (fst yx) (snd yx - 1));
    try
      let key = Curses.wgetch win in
      if key = 10 then str
      else if key = Curses.Key.backspace then (
        let new_str =
          if str = "" then str
          else String.sub str 0 (String.length str - 1)
        in
        Curses.clrtoeol ();
        parse_string win new_str echo )
      else parse_string win (str ^ String.make 1 (char_of_int key)) echo
    with _ -> parse_string win str echo

  let commit_msg_prompt : MState.printable =
    { text = "Enter your commit message: "; color = "green" }

  let results_header : MState.printable =
    { text = "Results: "; color = "green" }

  let diff_header : MState.printable =
    { text = "Diff results: "; color = "magenta" }

  let branch_options : MState.printable list =
    [
      { text = "b  checkout branch"; color = "green" };
      { text = "c  create branch"; color = "green" };
      { text = "x  delete branch"; color = "green" };
    ]

  let reset_options : MState.printable list =
    [
      { text = "h  reset hard"; color = "green" };
      { text = "s  reset soft"; color = "green" };
    ]

  let reset_prompt : MState.printable =
    { text = "Enter commit to reset to : "; color = "green" }

  let username_prompt : MState.printable =
    { text = "Enter username: "; color = "green" }

  let password_prompt : MState.printable =
    { text = "Enter password: "; color = "green" }

  let pull_elsewhere_msg_prompt : MState.printable =
    { text = "Enter branch to pull from: "; color = "green" }

  let push_elsewhere_msg_prompt : MState.printable =
    { text = "Enter branch to push to: "; color = "green" }

  let push_options : MState.printable list =
    [
      { text = "p  push to remote"; color = "green" };
      { text = "u  push origin/master"; color = "green" };
      { text = "e  push elsewhere"; color = "green" };
    ]

  let pull_options : MState.printable list =
    [
      { text = "p  pull from remote"; color = "green" };
      { text = "u  pull origin/master"; color = "green" };
      { text = "e  pull elsewhere"; color = "green" };
    ]

  let diff_options : MState.printable list =
    [
      { text = "t  tracked"; color = "green" };
      { text = "s  staged"; color = "green" };
      { text = "f  file"; color = "green" };
      { text = "a  all"; color = "green" };
    ]

  let get_branch_msg_prompt : MState.printable =
    { text = "Enter branch name: "; color = "green" }

  let stash_options : MState.printable list =
    [
      { text = "a  apply"; color = "green" };
      { text = "p  pop"; color = "green" };
    ]

  let blank_line : MState.printable = { text = " "; color = "white" }

  open Yojson.Basic.Util

  let to_printable_list =
    let printable_of_json json : MState.printable =
      {
        text = json |> member "text" |> to_string;
        color = json |> member "color" |> to_string;
      }
    in
    List.map printable_of_json

  let tutorial_from_json name =
    Yojson.Basic.from_file "tutorial.json"
    |> member name |> to_list |> to_printable_list

  let normal_tutorial : MState.printable list =
    tutorial_from_json "normal tutorial"

  let diff_tutorial : MState.printable list =
    tutorial_from_json "diff tutorial"

  let pull_tutorial : MState.printable list =
    tutorial_from_json "pull tutorial"

  let push_tutorial : MState.printable list =
    tutorial_from_json "push tutorial"

  let branch_tutorial : MState.printable list =
    tutorial_from_json "branch tutorial"

  let reset_tutorial : MState.printable list =
    tutorial_from_json "reset tutorial"

  let stash_tutorial : MState.printable list =
    tutorial_from_json "stash tutorial"

  let render_command_done state win msg =
    render_line win (MState.get_curs state) true blank_line;
    render_line win (MState.get_curs state) true results_header;
    render_line win (MState.get_curs state) true
      { text = msg; color = "white" }

  let render_normal state win render_curs =
    let curs = MState.get_curs state in
    Curses.werase win;
    screen := [||];
    top_line := 0;
    let lines = MState.printable_of_state state in
    cursor_reset win;
    let render_curs = MState.get_mode state <> CommitMode in
    render_lines win lines curs render_curs;
    match MState.get_mode state with
    | CommandDone msg -> render_command_done state win msg
    | PullMode ("m", "m", "m") ->
        render_line win curs true blank_line;
        render_lines win pull_options curs true
    | PushMode ("m", "m", "m") ->
        render_line win curs true blank_line;
        render_lines win push_options curs true
    | _ -> check_err (Curses.wrefresh win)

  let render_tutorial state win (mode : MState.render_mode) =
    render_normal state win true;
    match mode with
    | NormalTutorialMode ->
        render_lines win normal_tutorial (MState.get_curs state) true
    | DiffTutorialMode ->
        render_lines win
          ( [ blank_line ] @ diff_options @ [ blank_line ]
          @ diff_tutorial )
          (MState.get_curs state) true
    | PullTutorialMode ->
        render_lines win
          ( [ blank_line ] @ pull_options @ [ blank_line ]
          @ pull_tutorial )
          (MState.get_curs state) true
    | PushTutorialMode ->
        render_lines win
          ( [ blank_line ] @ push_options @ [ blank_line ]
          @ push_tutorial )
          (MState.get_curs state) true
    | BranchTutorialMode ->
        render_lines win
          ( [ blank_line ] @ branch_options @ [ blank_line ]
          @ branch_tutorial )
          (MState.get_curs state) true
    | StashTutorialMode ->
        render_lines win
          ( [ blank_line ] @ stash_options @ [ blank_line ]
          @ stash_tutorial )
          (MState.get_curs state) true
    | ResetTutorialMode ->
        render_lines win
          ( [ blank_line ] @ reset_options @ [ blank_line ]
          @ reset_tutorial )
          (MState.get_curs state) true
    | _ -> failwith "use of wrong renderer"

  let render_scroll_up st win =
    Curses.werase win;
    cursor_reset win;
    let scr = !screen in
    let max_y = fst (Curses.getmaxyx win) in
    let len = Array.length scr in
    if !top_line <= 1 || len < max_y then render_normal st win true
    else
      let new_top = !top_line - 1 in
      let new_btm = new_top + max_y - 1 in
      screen := [||];
      Curses.werase win;
      cursor_reset win;
      for i = new_top to new_btm do
        render_line win (MState.get_curs st - 1) true scr.(i)
      done;
      screen := scr;
      top_line := !top_line - 1;
      check_err (Curses.wrefresh win)

  let render_scroll_down st win =
    let btm_line = !top_line + fst (Curses.getmaxyx win) - 1 in
    if btm_line >= Array.length !screen then ()
    else
      let scr = !screen in
      screen := [||];
      Curses.werase win;
      cursor_reset win;
      for i = !top_line + 1 to btm_line do
        render_line win (fst (Curses.getmaxyx win) - 2) true scr.(i)
      done;
      top_line := !top_line + 1;
      screen := scr;
      check_err (Curses.wrefresh win)

  let diff_color str =
    let clr =
      if String.length str < 2 then "white"
      else
        let first_two = String.sub str 0 2 in
        if first_two = "++" || first_two = "--" then "white"
        else if first_two = "@@" then "cyan"
        else if String.sub first_two 0 1 = "+" then "green"
        else if String.sub first_two 0 1 = "-" then "red"
        else "white"
    in
    let line : MState.printable = { text = str; color = clr } in
    line

  let diff_to_lines str =
    String.split_on_char '\n' str |> List.map diff_color

  let render_diff_mode state win =
    render_normal state win true;
    match MState.get_mode state with
    | DiffMode str ->
        if str = "MENU" then (
          render_line win (MState.get_curs state) true blank_line;
          render_lines win diff_options (MState.get_curs state) true )
        else (
          render_line win (MState.get_curs state) true blank_line;
          render_line win (MState.get_curs state) true diff_header;
          render_lines win (diff_to_lines str) (MState.get_curs state)
            true )
    | _ -> failwith "Wrong render function: not in diff mode"

  let render_reset_mode state win =
    render_normal state win true;
    render_line win (MState.get_curs state) true blank_line;
    render_lines win reset_options (MState.get_curs state) true

  let render_stash_mode state win =
    render_normal state win true;
    render_line win (MState.get_curs state) true blank_line;
    render_lines win stash_options (MState.get_curs state) true

  let render_push_mode state win =
    render_normal state win true;
    render_line win (MState.get_curs state) true blank_line;
    render_lines win push_options (MState.get_curs state) true

  let render_pull_mode state win =
    render_normal state win true;
    render_line win (MState.get_curs state) true blank_line;
    render_lines win pull_options (MState.get_curs state) true

  let render_branch_mode state win =
    render_normal state win true;
    render_line win (MState.get_curs state) true blank_line;
    render_lines win branch_options (MState.get_curs state) true

  (** [render state win] prints all necessary information from [state]
      to window [win]. *)
  let rec render state win =
    match MState.get_curs_state state with
    | MState.OffScrUp -> render_scroll_up state win
    | MState.OffScrDown -> render_scroll_down state win
    | MState.OnScr -> (
        match MState.get_mode state with
        | DiffMode _ -> render_diff_mode state win
        | CommandDone _ -> render_normal state win true
        | PullMode ("m", "m", "m") -> render_normal state win true
        | PushMode ("m", "m", "m") -> render_normal state win true
        | PushMode (_, _, _) -> render_push_mode state win
        | PullMode (_, _, _) -> render_pull_mode state win
        | Normal -> render_normal state win true
        | CommitMode -> render_normal state win true
        | BranchMode -> render_branch_mode state win
        | NormalTutorialMode ->
            render_tutorial state win NormalTutorialMode
        | DiffTutorialMode -> render_tutorial state win DiffTutorialMode
        | PullTutorialMode -> render_tutorial state win PullTutorialMode
        | PushTutorialMode -> render_tutorial state win PushTutorialMode
        | BranchTutorialMode ->
            render_tutorial state win BranchTutorialMode
        | StashTutorialMode ->
            render_tutorial state win StashTutorialMode
        | ResetTutorialMode ->
            render_tutorial state win ResetTutorialMode
        | StashMode -> render_stash_mode state win
        | ResetMode -> render_reset_mode state win
        | _ -> render_normal state win true )

  let render_with_parse state win prompt =
    Curses.werase win;
    cursor_reset win;
    render_normal state win false;
    render_line win (MState.get_curs state) false blank_line;
    render_line win (MState.get_curs state) false prompt;
    let input = parse_string win "" (prompt <> password_prompt) in
    check_err (Curses.noecho ());
    input

  let render_push_pull_mode state win =
    try
      let prompt, u, p, b, pull =
        match MState.get_mode state with
        | PullMode ("", p, b) -> (username_prompt, "", p, b, true)
        | PullMode (u, "", b) -> (password_prompt, u, "", b, true)
        | PullMode (u, p, "") -> (get_branch_msg_prompt, u, p, "", true)
        | PullMode ("m", "m", "m") ->
            render_normal state win true;
            raise Command.Program_terminate
        | PushMode ("", p, b) -> (username_prompt, "", p, b, false)
        | PushMode (u, "", b) -> (password_prompt, u, "", b, false)
        | PushMode (u, p, "") -> (get_branch_msg_prompt, u, p, "", false)
        | PushMode ("m", "m", "m") ->
            render_normal state win true;
            raise Command.Program_terminate
        | _ ->
            failwith "Wrong render function: not in a push or pull mode"
      in
      let out = render_with_parse state win prompt in
      if
        (prompt = password_prompt && b <> "")
        || prompt = get_branch_msg_prompt
      then render (MState.update_mode state Command.Nop) win;
      out
    with _ -> ""

  let render_single_input_mode state win =
    let prompt =
      match MState.get_mode state with
      | CommitMode -> commit_msg_prompt
      | AllMode -> commit_msg_prompt
      | CheckoutGetBranchNameMode -> get_branch_msg_prompt
      | CreateGetBranchNameMode -> get_branch_msg_prompt
      | DeleteGetBranchNameMode -> get_branch_msg_prompt
      | ResetGetCommitMode _ -> reset_prompt
      | _ -> failwith "Wrong render function: not in an input mode"
    in
    let out = render_with_parse state win prompt in
    render (MState.update_mode state Command.Nop) win;
    out

  (** [render_input_mode state win] prints all necessary information
      from [state] to window [win] and retrieves input from the user. *)
  let render_input_mode state win =
    match MState.get_mode state with
    | PullMode (_, _, _) -> render_push_pull_mode state win
    | PushMode (_, _, _) -> render_push_pull_mode state win
    | _ -> render_single_input_mode state win
end
