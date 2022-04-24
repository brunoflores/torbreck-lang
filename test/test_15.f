Counter = {get: Unit -> Nat, inc: Unit -> Unit};
inc3 = lambda x: Counter. (x.inc unit; x.inc unit; x.inc unit);

c = let x = ref 1 in
      { get = (lambda _:Unit. !x),
        inc = (lambda _:Unit. x := succ (!x)) };

(inc3 c; c.get unit);
