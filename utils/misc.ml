let try_finally ?(always = fun () -> ()) ?(exceptionally = fun () -> ()) work =
  match work () with
  | result -> begin
      match always () with
      | () -> result
      | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
    end
  | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      begin
        match always () with
        | () ->
            exceptionally ();
            Printexc.raise_with_backtrace work_exn work_bt
        | exception always_exn ->
            let always_bt = Printexc.get_raw_backtrace () in
            exceptionally ();
            Printexc.raise_with_backtrace always_exn always_bt
      end
