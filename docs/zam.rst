The ZINC not-so-abstract machine
################################

- Krivine's machine with marks specialized to call-by-value only, and
- Extended to handle constants

Stack-based calling convention where functions may not consume all their
arguments, but then their result must be applied to the remaining
arguments.

Registers for the abstract machine
**********************************

+-------------+------------------------------------------------+
| pc          |the code pointer                                |
+-------------+------------------------------------------------+
| asp         |the stack pointer for the argument stack (grows |
|             |downward)                                       |
+-------------+------------------------------------------------+
| rsp         |stack pointer for the return stack (grows       |
|             |downward)                                       |
+-------------+------------------------------------------------+
| tp          |pointer to the current trap frame               |
+-------------+------------------------------------------------+
| env         |the remaining part (heap-allocated) of the      |
|             |environment                                     |
+-------------+------------------------------------------------+
| cache_size  |number of entries in the volatile part of the   |
|             |environment                                     |
+-------------+------------------------------------------------+
| accu        |the accumulator: this is used to hold           |
|             |intermediate results                            |
+-------------+------------------------------------------------+

"asp" and "rsp" are local copies of the global variables "extern_asp" and "extern_rsp".

Stacks
======

Krivine's machine split into two stacks:

- The **argument stack**: holds arguments to function calls, that is sequences
  of values, separated by marks
- The **return stack**: holds (unallocated) closures, that is pairs of a code pointer
  and an environment

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
