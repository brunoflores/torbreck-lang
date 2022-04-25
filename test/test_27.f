-- Streams.

Stream = Rec A. Unit -> { Nat, A };

hd = lambda s: Stream. (s unit).1;
tl = lambda s: Stream. ((s unit).2) as Stream;

upfrom0 = fix (lambda f: Nat -> Stream. lambda n: Nat. lambda _: Unit. { n, f (succ n) }) 0;

hd upfrom0;
hd (tl (tl (tl upfrom0)));
