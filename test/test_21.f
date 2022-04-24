Counter = { get: Unit -> Nat, inc: Unit -> Unit };

-- The "representation type" of the "class" below.
CounterRep = { x: Ref Nat };

-- Document in code that this record must type-check as a Counter.
counterClass =
  lambda r: CounterRep.
    { get = lambda _: Unit. !(r.x),
      inc = lambda _: Unit. r.x := succ (!(r.x)) } as Counter;

-- Instantiate a counter using a selected implementation.
newCounter =
  lambda _: Unit. let r = { x = ref 1 } in
    counterClass r;

c = newCounter unit;
