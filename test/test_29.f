-- Counter is a recursively defined _record_.
Counter = Rec C. { get: Nat, inc: Unit -> C, dec: Unit -> C };

-- An instance c of Counter.
c = let create = fix (lambda f: { x: Nat } -> Counter. lambda s: { x: Nat }.
                        { get = s.x,
                          inc = lambda _: Unit. f { x = succ (s.x) },
                          dec = lambda _: Unit. f { x = pred (s.x) } })
    in (create { x = 0 }) as Counter;

-- The Counters defined here are purely functional...
c1 = (c.inc unit) as Counter;
c2 = (c1.inc unit) as Counter;
c2.get;
