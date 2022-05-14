The ZINC not-so-abstract machine
################################

.. epigraph::

   The [ZINC]_ machine was specially designed to build less closures.

   -- Xavier Leroy

(Other names are *ZINC environment machine* and *ZAM*.)

We specify the execution model using an abstract machine.
Examples of abstract machines for strict functional languages are [SECD]_,
[FAM]_, and [CAM]_.

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
and :math:`\textbf{Grab}`. A term in de Bruijn's notation is compiled as
follows:

.. math::

   \textlbrackdbl n \textrbrackdbl = \textbf{Access}(n)

.. math::

   \textlbrackdbl (M N) \textrbrackdbl =
   \textbf{Push} (\textlbrackdbl N \textrbrackdbl) ;
   \textlbrackdbl M \textrbrackdbl

.. math::

   \textlbrackdbl \lambda M \textrbrackdbl =
   \textbf{Grab} ; \textlbrackdbl M \textrbrackdbl

The machine is equipped with

- a code pointer,
- a register holding the current environment (a list of closures, that is,
  pairs of code pointers and environments), and
- a stack of closures.

The transition function is as follows:

.. math::

   \begin{array}{|l l l|l l l|}
   \hline
     \text{Code} & \text{Env.} & \text{Stack} & \text{Code} & \text{Env.} &
       \text{Stack} \\
   \hline
     \textbf{Access}(0); c & (c_0, e_0) \cdot e & s & c_0 & e_0 & s \\
     \textbf{Access}(n+1); c & (c_0, e_0) \cdot e & s & \textbf{Access}(n); c
       & e & s \\
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
     \text{Code} & \text{Env.} & \text{Stack} & \text{Code} & \text{Env.} &
       \text{Stack} \\
   \hline
     \textbf{Access}(0); c & (c_0, e_0) \cdot e & s & c_0 & e_0 & s \\
     \textbf{Access}(n+1); c & (c_0, e_0) \cdot e & s & \textbf{Access}(n); c
       & e & s \\
     \textbf{Push}(c'); c & e & s & c & e & (c', e) \cdot s \\
     \textbf{Grab}; c & e & (c_0, e_0) \cdot s & c & (c_0, e_0) \cdot e & s \\
     \textbf{Grab}; c & e & \langle c_0, e_0 \rangle \cdot s & c_0 & e_0 &
       (\textbf{Grab}; c, e) \cdot s \\
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
expressions :math:`E` in tail-call position, that is expressions whose value
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
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} &
       \text{Return stack} \\
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
   \textbf{Push} ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ;
     \textbf{Appterm}

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
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} &
       \text{Return stack} \\
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

In tail-cal position, the :math:`\textbf{Grab}` instruction simply pops one
argument from the argument stack, and puts it in front of the environment.
If all arguments have already been consumed, that is if there is a mark at the
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
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} &
       \text{Return stack} \\
   \hline
     \textbf{Cur}(c_1); c_0 & a & e & s & r \\
     c_0 & (c_1, e) & e & s & r \\
   \hline
     \textbf{Grab}; c_0 & a & e_0 & \varepsilon .s & (c_1, e_1).r \\
     c_1 & (c_0, e_0) & e_1 & s & r \\
   \hline
     \textbf{Grab}; c_0 & a & e & v.s & r \\
     c & a & v.e & s & r \\
   \hline
     \textbf{Return}; c_0 & a & e_0 & \varepsilon .s & (c_1, e_1).r \\
     c_1 & a & e_1 & s & r \\
   \hline
     \textbf{Return}; c_0 & a = (c_1, e_1) & e_0 & v.s & r \\
     c_1 & a & v.e_1 & s & r \\
   \hline
   \end{array}

Local declarations
==================

.. math::

   \mathcal{T} \textlbrackdbl \texttt{let} \space \texttt{1} = N \space
   \texttt{in} \space M \textrbrackdbl = \mathcal{C} \textlbrackdbl N
   \textrbrackdbl ; \textbf{Let} ; \mathcal{T} \textlbrackdbl M \textrbrackdbl

.. math::

   \mathcal{C} \textlbrackdbl \texttt{let} \space \texttt{1} = N \space
   \texttt{in} \space M \textrbrackdbl = \mathcal{C} \textlbrackdbl N
   \textrbrackdbl ; \textbf{Let} ; \mathcal{C} \textlbrackdbl M \textrbrackdbl ;
   \textbf{Endlet}

.. math::

   \mathcal{T} \textlbrackdbl \texttt{let} \space \texttt{rec} \space
   \texttt{1} = N \space \texttt{in} \space M \textrbrackdbl = \textbf{Dummy} ;
   \mathcal{C} \textlbrackdbl N \textrbrackdbl ; \textbf{Update} ;
   \mathcal{T} \textlbrackdbl M \textrbrackdbl

.. math::

   \mathcal{C} \textlbrackdbl \texttt{let} \space \texttt{rec} \space
   \texttt{1} = N \space \texttt{in} \space M \textrbrackdbl = \textbf{Dummy} ;
   \mathcal{C} \textlbrackdbl N \textrbrackdbl ; \textbf{Update} ;
   \mathcal{C} \textlbrackdbl M \textrbrackdbl ; \textbf{Endlet}

The special case of :math:`\texttt{let}`, that is :math:`((\lambda x.M) N)`,
is so common that it deserves a faster and simpler compilation scheme than
actually applying an abstraction. It is enough to evaluate :math:`N` and
add its value to the environment, using the :math:`\textbf{Let}` instruction,
then to evaluate :math:`M` in this modified environment; then, the
:math:`\textbf{Endlet}` instruction restores the original environment,
if needed.

For recursive definitions, use the same trick suggested for the [CAM]_:
first, a dummy value is added to the environment (instruction
:math:`\textbf{Dummy}`), and :math:`N` is evaluated in this modified
environment; the dummy value is then physically updated with the actual value of
:math:`N` (instruction :math:`\textbf{Update}`). This may fail to reach a
fixpoint, since the physical update may be impossible (in case of an unboxed
value, an integer for instance). However, it works fine for the most commonly
used case: when :math:`M` is an abstraction :math:`\lambda .P`.

Primitives
==========

.. math::

   \mathcal{T} \textlbrackdbl p (M_1, \cdots , \cdots , M_k) \textrbrackdbl = \\
   \mathcal{C} \textlbrackdbl p (M_1, \cdots , \cdots , M_k) \textrbrackdbl = \\
   \mathcal{C} \textlbrackdbl M_k \textrbrackdbl ; \textbf{Push} ; \cdots
   \mathcal{C} \textlbrackdbl M_2 \textrbrackdbl ; \textbf{Push} ;
   \mathcal{C} \textlbrackdbl M_1 \textrbrackdbl ; \textbf{Prim} (p)

We write :math:`\textbf{Prim} (p)` for the instruction associated with the
primitive operation :math:`p` (e.g. :math:`+`, :math:`=`, :math:`\texttt{car}`).
This instruction takes its first argument in the accumulator, the remaining
arguments in the argument stack, and puts its result in the accumulator.

.. math::

   \begin{array}{|l|l|l|l|l|}
   \hline
     \text{Code} & \text{Accu} & \text{Env.} & \text{Arg. stack} &
       \text{Return stack} \\
   \hline
     \textbf{Prim}(p); c & a & e & v_2 \cdots v_k.s & r \\
     c & p(a, v_2, \cdots , v_k) & e & s & r \\
   \hline
   \end{array}

Environment representation
==========================

The ZINC machine was designed to build less closures. This opens the way for
less costly (in terms of heap allocation) representations of environments.

When we don't have to build any closures, the current environment does not have
to survive the evaluation of the current function body. We can store it, or part
of it, in some volatile location (stack or registers) that will be automatically
reclaimed when the current function returns. We can go even further: assuming
few closures are built, a sensible policy is to systematically put values
being added to the environment in one of these volatile locations, and to copy
them back to persistent storage (i.e. in the heap) when a closure is built.

In this approach, the environment
:math:`0 \leftarrow a_0, \cdots , n \leftarrow a_n` is represented by a
persistent part :math:`a_k, \cdots , a_n`, which is the environment part of
the closure most recently applied or built, and a volatile part
:math:`a_0, \cdots , a_{k-1}`, which holds values added to the environment
since then.

The linker and the runtime system
=================================

.. list-table:: Kinds of operands
   :header-rows: 0

   * - :math:`n`
     - a small integer (the size of an opcode)
   * - :math:`ofs`
     - an offset for a relative branch, relative to the address where it is
       stored; it uses two bytes
   * - :math:`tag`
     - the tag of a block (one byte)
   * - :math:`header`
     - a well-formed block header (four bytes)
   * - :math:`int_8`
     - a small integer constant (one byte)
   * - :math:`int_{16}`
     - a medium integer constant (two bytes)
   * - :math:`int_{32}`
     - a large integer constant (four bytes)
   * - :math:`float`
     - a floating-point number (four, eight or ten bytes, depending on the
       hardware)
   * - :math:`string`
     - a character string, stored as if it was in the heap

.. rubric:: Constants and literals

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Constbyte(int 8)`, :opcode:`Constshort(int 16)`,
       :opcode:`Constlong(int 32)`
     - Put an integer constant in the accumulator. :opcode:`Constlong` allows
       loading any constant, as long as it is not a pointer in the heap.
   * - :opcode:`Atom(n)`, :opcode:`Atom0`, :math:`\cdots`, :opcode:`Atom9`
     - Put a pointer to a zero-sized block tagged :math:`n` in the
       accumulator.
   * - :opcode:`GetGlobal(int 16)`, :opcode:`SetGlobal(int 16)`
     - Load (resp. store) the accumulator from the global variable number
       :math:`int_{16}`.

.. rubric:: Function handling

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Push`, :opcode:`Pushmark`
     - Push the accumulator (resp. a mark) on the argument stack.
   * - :opcode:`Apply`, :opcode:`Appterm`
     - Call (resp. jump to) the closure contained in the accumulator.
   * - :opcode:`Return`
     - If there is a mark on top of the argument stack, pop it and return to
       the caller; otherwise, jump to the closure contained in the accumulator.

.. rubric:: Environment handling

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Access(n)`, :opcode:`Access0`, :math:`\cdots`, :opcode:`Access5`
     - Fetch the :math:`n^{th}` slot of the environment, and put it in the
       accumulator.
   * - :opcode:`Let`
     - Put the value of the accumulator in front of the environment.
   * - :opcode:`Endlet(n)`, :opcode:`Endlet1`
     - Throw away the first :math:`n` local variables from the environment.
   * - :opcode:`Dummies(n)`
     - Put :math:`n` dummy closures in front of the environment.
   * - :opcode:`Update(n)`
     - Physically update the :math:`n^{th}` slot of the environment with
       the value of the accumulator.
   * - :opcode:`Letrec1(ofs)`
     - Same as :opcode:`Dummies(1); Closure(ofs); Update(0)`, a very frequent
       sequence, corresponding to
       :math:`\texttt{let rec f = function ... in ...}`

.. rubric:: Building and deconstructing block

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Makeblock(header)`, :opcode:`Makeblock1(tag)`, :math:`\cdots`,
       :opcode:`Makeblock4(tag)`
     - Allocate a block with a given header, initialise field 0 with the
       accumulator, and the remaining fields with values taken from the
       argument stack.
   * - :opcode:`Getfield(n)`, :opcode:`Getfield0`, :math:`\cdots`,
       :opcode:`Getfield3`
     - Access the :math:`n^{th}` field of the block pointed to by the
       accumulator.
   * - :opcode:`Setfield(n)`, :opcode:`Setfield0`, :math:`\cdots`,
       :opcode:`Setfield3`
     - Physically replace the :math:`n^{th}` field of the block pointed to
       by the accumulator with the value popped from the argument stack.

.. rubric:: Integers

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`SuccInt`, :opcode:`PredInt`, :opcode:`NegInt`,
       :opcode:`AddInt`, :opcode:`SubInt`, :opcode:`MulInt`, :opcode:`DivInt`,
       :opcode:`ModInt`, :opcode:`AndInt`, :opcode:`OrInt`, :opcode:`XorInt`,
       :opcode:`ShiftLeftInt`, :opcode:`ShiftRightInt`
     - Usual arithmetic operations on integers.

.. rubric:: Floating-point numbers

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Floatop(n)`
     - Allocates room for one floating point result, and executes the
       sub-instruction :math:`n`, one of :opcode:`AddFloat`, :opcode:`SubFloat`,
       :opcode:`MulFloat`, :opcode:`DivFloat`, and the usual transcendental
       functions.
   * - :opcode:`FloatOfInt`, :opcode:`IntOfFloat`
     - Conversion from and integer, and truncation to an integer.

.. rubric:: Strings

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Makestring`
     - Allocates a string of given length (in the accumulator).
   * - :opcode:`StringLength`
     - Length of the string contained in the accumulator.
   * - :opcode:`GetChar`, :opcode:`SetChar`
     - Read or modify one char in a string.
   * - :opcode:`FillString`, :opcode:`BlitString`
     - Fill a substring with a given character, or copy one substring into
       another.

.. rubric:: Predicates

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Boolnot`
     - Negation: returns "true" (the zero-sized block tagged 1) if the block
       in the accumulator is tagged 0, and "false" (the zero-sized block
       tagged 0) otherwise.
   * - :opcode:`Ed`, :opcode:`Equal`
     - Pointer equality (resp. structural equality) between the accumulator
       and the top of the stack.
   * - :opcode:`EqInt`, :opcode:`NeqInt`, :opcode:`LtInt`, :opcode:`GtInt`,
       :opcode:`LeInt`, :opcode:`GeInt`
     - Usual comparison predicates on integers.
   * - :opcode:`EqFloat`, :opcode:`NeqFloat`, :opcode:`LtFloat`,
       :opcode:`GtFloat`, :opcode:`LeFloat`, :opcode:`GeFloat`
     - Usual comparison predicates on floating-point numbers.
   * - :opcode:`EqString`, :opcode:`NeqString`, :opcode:`LtString`,
       :opcode:`GtString`, :opcode:`LeString`, :opcode:`GeString`
     - Usual comparison predicates on strings.

.. rubric:: Branches and conditional branches

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`Branch(ofs)`
     - Unconditional relative jump.
   * - :opcode:`BranchIf(ofs)`, :opcode:`Branchifnot(ofs)`,
       :opcode:`Branchifeqtag(tag,ofs)`, :opcode:`Branchifneqtag(tag,ofs)`
     - Conditional branches on the tag :math:`t` of the block pointed to
       by the accumulator: :opcode:`Branchif` jumps if :math:`t \ne 0`,
       :opcode:`Branchifnot` jumps if :math:`t = 0`, :opcode:`Branchifeqtag`
       jumps if :math:`t = tag`, :opcode:`Branchifneqtag` jumps if
       :math:`t \ne tag`.
   * - :opcode:`Switch(ofs 0,...,ofs k)`
     - Jumps to the offset :math:`ofs_t`, where :math:`t` is the tag :math:`t`
       of the block contained in the accumulator.
   * - :opcode:`BranchifEq(ofs)`, :opcode:`BranchifNeq(ofs)`,
       :opcode:`BranchifEqual(ofs)`, :opcode:`BranchifNequal(ofs)`,
       :opcode:`BranchifLtInt(ofs)`, :math:`\cdots`,
       :opcode:`BranchifGetString(ofs)`
     - Conditional branches corresponding to the binary predicates above.
   * - :opcode:`BranchIfNeqImmInt(int 32,ofs)`,
       :opcode:`BranchifNeqImmFloat(float,ofs)`,
       :opcode:`BranchIfNeqImmString(string,ofs)`
     - Compare the accumulator with the constant given as argument,
       and jumps if different (Useful for fast pattern matching).

.. rubric:: Miscellaneous

.. list-table::
   :header-rows: 0
   :widths: 40 60

   * - :opcode:`CCall0(n)`, :math:`\cdots`, :opcode:`CCall5(n)`
     - Call a C function, with 0 to 5 arguments. C functions are put in a
       special table; :math:`n` is the number of the desired function.
       The firest argument is the value of the accumulator, the remaining
       arguments are popped from the argument stack. The result is put in the
       accumulator.
   * - :opcode:`StartFun`
     - Perform various checks such as stack overflow, pending break condition,
       and so on. Intended to be inserted at the beginning of each function and
       loop body.
   * - :opcode:`Nop1`, :opcode:`Nop2`, :opcode:`Nop3`
     - Do nothing, but skip respectively one, two, and three bytes.
       Used to align code on 16-bit or 32-bit boundaries.

.. rubric:: Footnotes

.. [ZINC]
.. [CAM]
.. [FAM]
.. [SECD]
