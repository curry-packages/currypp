------------------------------------------------------------------------
The preprocessor contains a transformation tool to generate
a Curry module with assertion checking from pre/postconditions
and specifications.

The tool is implemented in the module `CPP.Contracts`.

------------------------------------------------------------------------
How to use the tool:

The tool is integrated into the Curry Preprocessor (currypp). Hence,
to activate the transformation, put the following line into the
beginning of your Curry module:

{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=contracts #-}

If postconditions or specifications are nondeterministic,
it is better that they will be evaluated via encapsulated search
(see `examples/Contracts/NDAssertion.curry` for a discussion).
For this purpose use the option "-e" as follows:

{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=contracts --optF=-e #-}

------------------------------------------------------------------------
Options:

The contract preprocessor accepts the following options:

-o : write the transformed Curry program into file <prog>.curry.CURRYPP
-e : encapsulate nondeterminism of assertions
-t : assert contracts only to top-level (but not to direct recursive) calls;
     this might lead to a faster execution but less (incomplete!)
     contract checking

------------------------------------------------------------------------
Assumptions:

- Naming conventions: for an operation f,
  * its precondition must have the name f'pre
  * its postcondition must have the name f'post
  * its specification must have the name f'spec (or f'specd if the
    specification is deterministic which provides for stronger checking)

- Pre/postconditions or specifications are not required and could be
  omitted. However, if they are present, there must also be a defined
  operation to which they refer.

- If there is a postcondition f'post but no precondition f'pre,
  it is assumed that the precondition is defined as (const True).

- If there is a specification f'spec and an implementation of function f,
  the specification will be used as a postcondition for f.

- Any pre- or postcondition for a function f will be added to the
  implementation of f so that it will be checked at run-time.

- As a default, the complete computed value of a function will be
  checked in a postcondition. Alternatively, one can also check
  only some observed part of the computed result. For this purpose,
  one can define a function f'post'observe that maps each computed
  result into some observed part (see Examples/FibInfinite.curry
  for an example of this use).

- Pre- and postconditions are always checked in a strict manner,
  i.e., they might influence the evaluation of the original program.
  In a future version, a lazy assertion checking might be supported.

------------------------------------------------------------------------
Examples:

See programs in the directory `examples/Contracts`.

------------------------------------------------------------------------
Known problems:

- Strict assertions make operations stricter, thus, changing the run-time
  behavior.

------------------------------------------------------------------------
