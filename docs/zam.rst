The ZINC not-so-abstract machine
################################

(Other names are *ZINC environment machine* and *ZAM*.)

We specify the execution model using an abstract machine.
Examples of abstract machines for strict functional languages are SECD [#]_,
FAM [#]_ and CAM [#]_.

ZAM, the ZINC abstract machine, has a requirement that multiple application to
:math:`k` arguments should be efficient, almost as efficient as applying
a :math:`k` -ary function. This is not the case with the machines referenced
above , since they build :math:`k` - 1 intermediate closures to evaluate
this multiple application.

Why care? Maybe this bad behaviour is unavoidable and these closures are the
price to pay for the additional flexibility of curried functions.

Krivine's machine
=================

This machine performs reduction to *weak head normal form* following the
standard (leftmost-outermost) strategy. However, it represents
:math:`\lambda` -terms with closures, hence it does not perform substitutions
on the fly, but delays them until it reduces variables.

It has but three instructions: :math:`Access`, :math:`Push`, and :math:`Grab`.

ZAM is:

- Krivine's machine with marks specialised to call-by-value only, and
- Extended to handle constants

Stack-based calling convention where functions may not consume all their
arguments, but then their result must be applied to the remaining
arguments.

Registers for the abstract machine
==================================

.. list-table::
   :header-rows: 0

   * - :literal:`pc`
     - code pointer
   * - :literal:`asp`
     - stack pointer for the argument stack (grows downward)
   * - :literal:`rsp`
     - stack pointer for the return stack (grows downward)
   * - :literal:`tp`
     - pointer to the current trap frame
   * - :literal:`env`
     - remaining part (heap-allocated) of the environment
   * - :literal:`cache_size`
     - number of entries in the volatile part of the environment
   * - :literal:`accu`
     - accumulator to hold intermediate results

Stacks
======

Krivine's machine split into two stacks:

- The **argument stack**: holds arguments to function calls, that is sequences
  of values, separated by marks
- The **return stack**: holds (unallocated) closures, that is pairs of a code
  pointer and an environment

Accessing local variables
=========================

The compilation scheme for the local variable of index :math:`n` is:

.. math::

   \mathcal{T} \textlbrackdbl n \textrbrackdbl =
   \mathcal{C} \textlbrackdbl n \textrbrackdbl =
   Access(n)

The :math:`Access` instruction has the following semantics:

.. list-table::
   :header-rows: 1

   * - Code
     - Accu
     - Env
     - Argument stack
     - Return stack

   * - :math:`Access(n); c`
     - :math:`a`
     - :math:`e=v_0 \cdots v_n \cdots`
     - :math:`s`
     - :math:`r`

   * - :math:`c`
     - :math:`v_n`
     - :math:`e`
     - :math:`s`
     - :math:`r`

Application
===========

.. math::

   \mathcal{T} \textlbrackdbl ( M N_1 \cdots N_k ) \textrbrackdbl =
   \mathcal{C} \textlbrackdbl N_k \textrbrackdbl ; Push ; \cdots ; \mathcal{C} \textlbrackdbl N_1 \textrbrackdbl ;
   Push ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ; Appterm

.. math::

   \mathcal{C} \textlbrackdbl ( M N_1 \cdots N_k ) \textrbrackdbl =
   Pushmark; \mathcal{C} \textlbrackdbl N_k \textrbrackdbl ; Push ; \cdots ; \mathcal{C} \textlbrackdbl N_1 \textrbrackdbl ;
   Push ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ; Apply

Tail applications are treated as in Krivine's machine, since there is no need to
allocate a new argument stack by pushing a mark. The :math:`Appterm` instruction
takes care of consing the first argument with the environment of the closure;
this way, we do not have to put a :math:`Grab` instruction at the beginning
of each function. For other applications, we must push a mark on the argument
stack to separate the "new" arguments and force reduction to weak normal form.

.. |br| raw:: html

   <br />

.. list-table::
   :header-rows: 1

   * - Code
     - Accu
     - Env
     - Argument stack
     - Return stack

   * - :math:`Appterm; c_0` |br| :math:`c_1`
     - :math:`a=(c_1,e_1)` |br| :math:`a`
     - :math:`e_0` |br| :math:`v.e_1`
     - :math:`v.s` |br| :math:`s`
     - :math:`r` |br| :math:`r`

Abstractions
============

.. math::

   \mathcal{T} \textlbrackdbl \lambda E \textrbrackdbl =
   Grab ; \mathcal{T} \textlbrackdbl E \textrbrackdbl

.. math::

   \mathcal{C} \textlbrackdbl \lambda E \textrbrackdbl =
   Cur ( \mathcal{T} \textlbrackdbl E \textrbrackdbl ; Return )

In tail-cal position, the :math:`Grab` instruction simply pops one argument
from the argument stack, and puts it in front of the environment. If all
arguments have already been consumed, that is if there is a mark at the
top of the stack, it builds the closure of the current code with the current
environment and returns it to the called, while popping the mark.

.. rubric:: Footnotes

.. [#]
.. [#]
.. [#]
