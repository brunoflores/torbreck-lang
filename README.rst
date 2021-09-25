fulluntyped
================

Chapters 5, 6 and 7 of Types and Programming Languages, by Benjamin C. Pierce.

.. sh::
   $ dune build ./bin/main.exe

   $ dune exec ./bin/main.exe -- --help

   "In the mid 1960s, Peter Landin observed that a complex programming language can be understood by formulating it as a tiny core calculus capturing the language's essential mechanisms, together with a collection of convenient _derived forms_ whose behavior is understood by translating them into the core (Landin 1964, 1965, 1966; also see Tennent 1981). The core language used by Landin was the _lambda-calculus_, a formal system invented in the 1920s by Alonzo Church (1936, 1941), in which all computation is reduced to the basic operations of function definition and application. Following Landin's insight, as well as the pioneering work of John McCarthy on Lisp (1959, 1981), the lambda-calculus has seen widespread use in the specification of programming language features, in language design and implementation, and in the study of type systems. Its importance arises from the fact that it can be viewed simultaneously as a simple programming language _in which_ computations can be described and as a mathematical object _about which_ rigorous statements can be proved."

The "full" in "fulluntyped" stands for some enrichments added to the lambda-calculus:

* Numbers 
* Tuples 
* Records

The syntax of the lambda-calculus comprises just three sorts of terms:

.. math::

   \frac{ \sum_{t=0}^{N}f(t,k) }{N}
