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

It has but three instructions: :math:`\textbf{Access}`, :math:`\textbf{Push}`,
and :math:`\textbf{Grab}`. A term in de Bruijn's notation is compiled as follows:

.. math::

   \textlbrackdbl n \textrbrackdbl = \textbf{Access}(n)

.. math::

   \textlbrackdbl (M N) \textrbrackdbl = \textbf{Push} (\textlbrackdbl N \textrbrackdbl) ; \textlbrackdbl M \textrbrackdbl

.. math::

   \textlbrackdbl \lambda M \textrbrackdbl = \textbf{Grab} ; \textlbrackdbl M \textrbrackdbl

The machine is equipped with

- a code pointer,
- a register holding the current environment (a list of closures, that is,
  pairs of code pointers and environments), and
- a stack of closures.

The transition function is as follows:

.. math::

   \begin{array}{|l l l|l l l|}
   \hline
     \text{Code} & \text{Env.} & \text{Stack} & \text{Code} & \text{Env.} & \text{Stack} \\
   \hline
     \textbf{Access}(0); c & (c_0, e_0) \cdot e & s & c_0 & e_0 & s \\
     \textbf{Access}(n+1); c & (c_0, e_0) \cdot e & s & \textbf{Access}(n); c & e & s \\
     \textbf{Push}(c'); c & e & s & c & e & (c', e) \cdot s \\
     \textbf{Grab}; c & e & (c_0, e_0) \cdot s & c & (c_0, e_0) \cdot e & s \\
   \hline
   \end{array}

At all times the stack represents the *spine* of the term being reduced
(that is, the them whose code is in the code pointer).
The :math:`\textbf{Push}` instruction performs one step of unrolling, and
:math:`\textbf{Grab}` corresponds to one step of :math:`\beta` -reduction,
that is it records the substitution in the environment.

Krivine's machine with marks on the stack
=========================================

To perform strict evaluation with some variant of Krivine's machine,
we need first to be able to reduce some subterms of a given term to weak head
normal form. The problem with Krivine's machine is that it does not stop until
the stack is empty.

What we need is a way to stop reduction even if there are
arguments available on the stack. To this end, let's put a *mark* on some of the
closures awaiting in the stack; this mark says "don't put me in the environment,
stop reducing, and resume another reduction".

The modified Krivine's machine has a fourth instruction,
:math:`\textbf{Reduce}(c)`, to force reduction of :math:`c` to weak head normal
form, and a different semantics for :math:`\textbf{Grab}`. In the following,
marked closures are written :math:`\langle c, e \rangle` instead of :math:`(c, e)`.

.. math::

   \begin{array}{|l l l|l l l|}
   \hline
     \text{Code} & \text{Env.} & \text{Stack} & \text{Code} & \text{Env.} & \text{Stack} \\
   \hline
     \textbf{Access}(0); c & (c_0, e_0) \cdot e & s & c_0 & e_0 & s \\
     \textbf{Access}(n+1); c & (c_0, e_0) \cdot e & s & \textbf{Access}(n); c & e & s \\
     \textbf{Push}(c'); c & e & s & c & e & (c', e) \cdot s \\
     \textbf{Grab}; c & e & (c_0, e_0) \cdot s & c & (c_0, e_0) \cdot e & s \\
     \textbf{Grab}; c & e & \langle c_0, e_0 \rangle \cdot s & c_0 & e_0 & (\textbf{Grab}; c, e) \cdot s \\
     \textbf{Reduce}(c'); c & e & s & c' & e & \langle c, e \rangle \cdot s \\
   \hline
   \end{array}

The ZINC machine
================

The ZAM is

- Krivine's machine with marks specialised to call-by-value only, and
- Extended to handle constants

Stack-based calling convention where functions may not consume all their
arguments, but then their result must be applied to the remaining
arguments.

.. list-table:: Registers for the abstract machine
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

Krivine's machine split into two stacks,

- The **argument stack**: holds arguments to function calls, that is sequences
  of values, separated by marks
- The **return stack**: holds (unallocated) closures, that is pairs of a code
  pointer and an environment

Two compilation schemes: one, written
:math:`\mathcal{T} \textlbrackdbl E \textrbrackdbl`, is only valid for
expressions :math:`E` in tail-call position, that is expressions whose value is
is the value of the function body being evaluated; the other, written
:math:`\mathcal{C} \textlbrackdbl E \textrbrackdbl`, is always valid, but
usually less efficient.

The transitions of the ZAM corresponding to the generated instructions are given
below. The first line is the state before the transition, the second one is
the state after the transition.

Accessing local variables
=========================

The compilation scheme for the local variable of index :math:`n` is:

.. math::

   \mathcal{T} \textlbrackdbl n \textrbrackdbl =
   \mathcal{C} \textlbrackdbl n \textrbrackdbl =
   \textbf{Access}(n)

The :math:`Access` instruction has the following semantics:

.. math::

   \begin{array}{|l|l|l|l|l|}
   \hline
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} & \text{Return stack} \\
   \hline
     \textbf{Access}(n); c & a & e = v_0 \cdots v_n \cdots & s & r \\
     c & v_n & e & s & r \\
   \hline
   \end{array}

Application
===========

.. math::

   \mathcal{T} \textlbrackdbl ( M N_1 \cdots N_k ) \textrbrackdbl =
   \mathcal{C} \textlbrackdbl N_k \textrbrackdbl ; \textbf{Push} ; \cdots ;
   \mathcal{C} \textlbrackdbl N_1 \textrbrackdbl ;
   \textbf{Push} ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ; \textbf{Appterm}

.. math::

   \mathcal{C} \textlbrackdbl ( M N_1 \cdots N_k ) \textrbrackdbl =
   \textbf{Pushmark}; \mathcal{C} \textlbrackdbl N_k \textrbrackdbl ;
   \textbf{Push} ; \cdots ; \mathcal{C} \textlbrackdbl N_1 \textrbrackdbl ;
   \textbf{Push} ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ; \textbf{Apply}

Tail applications are treated as in Krivine's machine, since there is no need to
allocate a new argument stack by pushing a mark. The :math:`Appterm` instruction
takes care of consing the first argument with the environment of the closure;
this way, we do not have to put a :math:`Grab` instruction at the beginning
of each function. For other applications, we must push a mark on the argument
stack to separate the "new" arguments and force reduction to weak normal form.

.. math::

   \begin{array}{|l|l|l|l|l|}
   \hline
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} & \text{Return stack} \\
   \hline
     \textbf{Appterm}; c_0 & a = (c_1, e_1) & e_0 & v.s & r \\
     c_1 & a & v.e_1 & s & r \\
   \hline
     \textbf{Apply}; c_0 & a = (c_1, e_1) & e_0 & v.s & r \\
     c_1 & a & v.e_1 & s & (c_0, e_0).r \\
   \hline
     \textbf{Push}; c_0 & a & e & s & r \\
     c_0 & a & e & a.s & r \\
   \hline
     \textbf{Pushmark}; c_0 & a & e & s & r \\
     c_0 & a & e & \varepsilon .s & r \\
   \hline
   \end{array}

Abstractions
============

.. math::

   \mathcal{T} \textlbrackdbl \lambda E \textrbrackdbl =
   \textbf{Grab} ; \mathcal{T} \textlbrackdbl E \textrbrackdbl

.. math::

   \mathcal{C} \textlbrackdbl \lambda E \textrbrackdbl =
   \textbf{Cur} ( \mathcal{T} \textlbrackdbl E \textrbrackdbl ;
   \textbf{Return} )

In tail-cal position, the :math:`\textbf{Grab}` instruction simply pops one argument
from the argument stack, and puts it in front of the environment. If all
arguments have already been consumed, that is if there is a mark at the
top of the stack, it builds the closure of the current code with the current
environment and returns it to the called, while popping the mark.

Otherwise, we could push a mark, to allocate a new argument stack, and then do
the same thing. Of course, :math:`\textbf{Grab}` would always fail and return
immediately the desired closure. To save pushing a mark, and then immediately
test it, we use the cheaper :math:`\textbf{Cur}` instruction, in this case.

The :math:`\textbf{Return}` instruction that terminates the body of a function
does not simply jump back to the caller. It is actually the symmetric of
:math:`\textbf{Grab}`: it has to check if the argument stack is "empty"
(i.e. if the top of stack is a mark). If this is the case, it destroys the mark
and returns to the caller. But otherwise, it applies the result of the function
(necessarily a closure, if the original program is well-typed) to the remaining
arguments. This situation is the converse of partial application: a single
function is given more argument than it can use. This is the case of the
identity function in the following example:

.. math::

   ((\lambda x.x) (\lambda y.y + 1) \space 4)

.. math::

   \begin{array}{|l|l|l|l|l|}
   \hline
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} & \text{Return stack} \\
   \hline
     \textbf{Cur}(c_1); c_0 & a & e & s & r \\
     c_0 & (c_1, e) & e & s & r \\
   \hline
     \textbf{Grab}; c_0 & a & e_0 & \varepsilon .s & (c_1, e_1).r \\
     c_1 & (c_0, e_0) & e_1 & s & r \\
   \hline
     \textbf{Grab}; c_0 & a & e & \upsilon .s & r \\
     c & a & \upsilon .e & s & r \\
   \hline
     \textbf{Return}; c_0 & a & e_0 & \varepsilon .s & (c_1, e_1).r \\
     c_1 & a & e_1 & s & r \\
   \hline
     \textbf{Return}; c_0 & a = (c_1, e_1) & e_0 & \upsilon .s & r \\
     c_1 & a & \upsilon .e_1 & s & r \\
   \hline
   \end{array}

.. rubric:: Footnotes

.. [#]
.. [#]
.. [#]
