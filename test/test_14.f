c = let x = ref 1 in
      { get = (lambda _:Unit. !x),
        inc = (lambda _:Unit. x := succ (!x)) };
