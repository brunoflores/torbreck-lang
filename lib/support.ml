open Format

module Error = struct
  exception Exit of int

  type info =
    | INFO of {
        pos_fname : string;
        pos_lnum : int;
        pos_bol : int;
        pos_cnum : int;
      }
    | DUMMY
  [@@deriving show]

  type 'a withinfo = { i : info; v : 'a }

  let dummyinfo = DUMMY

  let createInfo fname lnum bol cnum =
    INFO { pos_fname = fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum }

  let printInfo ?context fi =
    match fi with
    | INFO { pos_fname; pos_lnum; pos_bol; _ } ->
        let first_line, last_line =
          match context with
          | Some (INFO { pos_lnum = first; _ }) -> (first, pos_lnum)
          | _ -> (pos_lnum, pos_lnum)
        in
        let print_carrot_line _ =
          let width_num_col = (String.length @@ string_of_int pos_lnum) + 2 in
          for _ = 0 to pos_bol + width_num_col do
            Printf.printf " "
          done;
          Printf.printf "^"
        in
        let lines = Stdio.In_channel.read_lines pos_fname in
        let print_line n =
          Printf.printf "%d | %s\n" n @@ List.nth lines (n - 1)
        in
        print_newline ();
        for n = first_line to last_line do
          print_line n
        done;
        print_carrot_line ();
        print_newline ()
    | DUMMY -> Printf.printf "<Unknown file information>"

  let errf f =
    print_flush ();
    open_vbox 0;
    open_hvbox 0;
    f ();
    print_cut ();
    close_box ();
    print_newline ();
    raise (Exit 1)

  let errfAt ?context fi f =
    errf (fun () ->
        printInfo fi ?context;
        print_space ();
        f ())

  let err s =
    errf (fun () ->
        print_string "Error: ";
        print_string s;
        print_newline ())

  let error ?context fi s =
    errfAt fi
      (fun () ->
        print_string s;
        print_newline ())
      ?context

  let warning s =
    print_string "Warning: ";
    print_string s;
    print_newline ()

  let warningAt fi s =
    printInfo fi;
    print_string " Warning: ";
    print_string s;
    print_newline ()
end
