-- Typing error inside let.

CounterRep = { y: Ref Nat };
c = let r = { x = ref 1 } as CounterRep in r;
