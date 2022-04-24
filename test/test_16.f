/* This is a multi-line
   comment. The lexer discards it. */
T = Nat->Nat;
lambda f:T. lambda x:Nat. f (f x);
