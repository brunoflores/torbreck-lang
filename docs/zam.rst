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

.. graphviz::

   digraph stacks {
     node_stacks [shape=record label="{Argument stack|Return stack}"]
   }

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
