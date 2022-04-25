-- A simple form of processes: functions that accept
-- a number and return a number and a new process.

Process = Rec A. Nat -> { Nat, A };

-- A helper plus
plus = fix (lambda s: Nat -> Nat -> Nat.
              lambda x: Nat. lambda y: Nat.
                if iszero y
                then x
                else s (succ x) (pred y));

-- A process p
p = fix (lambda f: Nat -> Process. lambda acc: Nat. lambda n: Nat.
           let newacc = plus acc n in
           { newacc, f newacc }) 0;

-- Auxiliary functions
curr = lambda s: Process. (s 0).1;
send = lambda n: Nat. lambda s: Process. ((s n).2) as Process;

curr (send 20 (send 3 (send 5 p)));
