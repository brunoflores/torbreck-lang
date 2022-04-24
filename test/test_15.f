Counter = {get: Unit -> Nat, inc: Unit -> Unit};
inc3 = lambda x: Counter. (x.inc unit; x.inc unit; x.inc unit);
