-- Recursive types: list.

NatList = Rec X. <nil: Unit, cons: { Nat, X }>;

-- Some built-in operators on lists from simpler parts:

-- A constant nil:
nil = <nil = unit> as NatList;

-- A constructor cons for adding an element to the front
-- of an existing list:
cons = lambda n: Nat. lambda l: NatList. <cons = { n, l }> as NatList;

-- An isnil operation and destructors hd and tl for extracting
-- the head and tail of a non-empty list (in terms of case):
isnil = lambda l: NatList. case l of
                             <nil = u> ==> true
                           | <cons = p> ==> false;

hd = lambda l: NatList. case l of
                          <nil = u> ==> 0
                        | <cons = p> ==> p.1;

tl = lambda l: NatList. case l of
                          <nil = u> ==> l
                        | <cons = p> ==> (p.2) as NatList;

-- Example:

plus = fix (lambda s: Nat -> Nat -> Nat.
              lambda x: Nat. lambda y: Nat.
                if iszero y
                then x
                else s (succ x) (pred y));

sumlist = fix (lambda s: NatList -> Nat. lambda l: NatList.
                 if isnil l then 0 else plus (hd l) (s (tl l)));

mylist = cons 2 (cons 3 (cons 5 nil));
sumlist mylist;
