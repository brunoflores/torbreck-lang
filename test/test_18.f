-- A ResetCounter is like a Counter but with an additional reset method.
ResetCounter = { get: Unit -> Nat, inc: Unit -> Unit, reset: Unit -> Unit };

newResetCounter = 
  lambda _: Unit. let x = ref 1 in
                    { get = lambda _: Unit. !x,
                      inc = lambda _: Unit. x := succ (!x),
                      reset = lambda _: Unit. x := 1 };

-- Subtyping: the domain of inc3 is Counter, 
-- but we'll use a ResetCounter instead.
Counter = {get: Unit -> Nat, inc: Unit -> Unit};
inc3 = lambda x: Counter. (x.inc unit; x.inc unit; x.inc unit);

rc = newResetCounter unit;
(inc3 rc; rc.reset unit; inc3 rc; rc.get unit);
