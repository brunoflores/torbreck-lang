-- A record that fails to type-check due to missing field.

Counter = { get: Unit -> Nat, inc: Unit -> Unit };
c = let r = { x = ref 1 } in
      { get = lambda _: Unit. !(r.x) } as Counter;
