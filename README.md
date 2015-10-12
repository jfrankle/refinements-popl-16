# EXAMPLE-DIRECTED SYNTHESIS: A TYPE-THEORETIC INTERPRETATION

Jonathan Frankle    (Princeton University)
Peter-Michael Osera (Grinnell College)
Dave Walker         (Princeton University)
Steve Zdancewic     (University of Pennsylvania)


## DEPENDENCIES
This repository requires several other packages to run.  On Ubuntu:
make, mono-devel, nuget, fsharp

To download the proper nuget packages, you might need to update your
certificates:
    > mozroots --import --sync


## INTRODUCTION
Welcome to the implementation of our refinement-based synthesis system.  It
consists of approximately 4,000 lines of F#.  It allows a user to supply a
collection of "libraries" (available declarations) in a polymorphic subset of
OCaml and, given a type and a refinement, ask the synthesizer to generate a
program.  These programs are written in a subset of OCaml (with an explicit
"fix" operator replacing nested "let rec" bindings for convenience).  We
describe the syntax and structure of synthesis programs in later sections.

Note that all data in the paper was collected an AWS m4.large instance.  The
performance on a VM will be scaled down accordingly.


## FILES
The main directory stores several files and directories:
  * refinements.sln: The F# solution file for the project.
  * Makefile: Makefile for building/cleaning the executable.
  * check-baseline.py, generate-baseline.py, collect-data.py: Python
     scripts for batch testing of the synthesis system.
  * synthesize.sh: Script that calls the executable for the synthesizer.
  * refinements: Source directory.
  * tests: Benchmarks for the system and baselines for what they should
     synthesize.


## ENUMERATION STRATEGIES
The synthesizer allows a user to choose between four different strategies for
resolving the non-determinism of the sequent calculus when searching for
arguments for function application (Strategies 1-4 in the paper):
  * Strategy 1: union_everywhere: Disjunction-based enumeration
  * Strategy 2: union_at_base: Hybrid enumeration
  * Strategy 3: union_everywhere_unsound: Unsound, disjunction-based enumeration
  * Strategy 4: raw: Type-based enumeration


## BENCHMARKS
We supply three sets of benchmarks (those presented in the paper).  They
consist of problems similar to those that might be assigned in an introductory
functional programming class. The three sets are:
  * popl-main: Drawn directly from those tested in the Myth paper (Osera and
     Zdancewic, PLDI 2015).  
  * popl-min: A subset of benchmarks from popl-main whose refinements were
     condensed using the more expressive refinement language in this paper
     except for polymorphism.
  * popl-poly: A subset of benchmarks from popl-min whose refinements were
     condensed using the more expressive refinement language in this paper
     including polymorphism.

Names listed are subdirectories of tests (i.e., tests/popl-min).
Each benchmark is stored in a file with extension .ml. Each baseline of the
result it should produce is stored in a file with extension .out.


## COMPILATION
To compile:
    > make

To clean:
    > make clean

To clean and remove nuget packages:
    > make nuke

## EXECUTION
To execute the synthesizer:
  1. Begin in the top-level directory (you should see the files in section 2).
  2. If you have not compiled already, compile.
    > make
  3. Choose the name of a file to run (tests/popl-main/list-map.ml).
  4. Choose an enumeration strategy (union_everywhere_unsound).
  5. Execute the synthesizer with your chosen file and enumeration strategy:
  
    > ./synthesize.sh --input [input file] --enum_strategy [strategy]
    
     For example:
     
    > ./synthesize.sh --input tests/popl-main/list-map.ml
                      --enum_strategy union_everywhere_unsound


## FLAGS
Each flag for the synthesizer and its options are listed below.
    --input [file]
      file: The name of the source file for the synthesis problem.
      *this flag must be set*

    --output [location]
      location: The name of an output file or stdout
      *defaults to stdout*

    --verbose [option]
      option: yes to display a trace of the search process at every step or no
      *defaults to no*

    --sampling [strategy]
      strategy: jit for just-in-time sampling or aot for ahead-of-time sampling
      *defaults to jit*

    --time [mode]
      mode: no to display no timing information, yes to output synthesis time in
            ms, or data to collect several values in comma-separated form
            test_name,refinement_size,synthesis_time,enumeration_cache_hit_ratio
      *defaults to no*

    --enum_strategy [strategy]
      strategy:raw, union_everywhere, union_at_base, union_everywhere_unsound
      *defaults to raw*

For the test run in the paper, all of the flags were set to their defaults
except for --enum_strategy and --input, which we varied, and --time, which was
set to data.

## BASELINES AND DATA
The python scripts generate-baselines.py and check-baselines.py create and
check against baseline files.  check-baselines.py runs a diff of the existing
baselines and the output of the synthesizer.  The python script collect-data.py
generates test data on stdout with the time flag set to data. 

To run generate-baselines.py:

    > python generate-baselines.py ./synthesize.sh [test(s)]

where [test(s)] is the path to a single test or a directory of tests to generate
multiple baselines at once.

To run check-baselines.py:

    > python check-baselines.py ./synthesize.sh [test(s)]

where [test(s)] is the path to a single test or a directory of tests to check
multiple baselines at once.

To run collect-data.py:

    > python collect-baselines.py ./synthesize.sh [test(s)]

where [test(s)] is the path to a single test or a directory of tests to collect
data on at once.


## SYNTHESIS PROBLEM
A synthesis problem consists of a series of datatype declarations and library
declarations followed by a synthesis problem.


## DATATYPE DECLARATIONS
Dataype declarations are identical to those in OCaml.  Each datatype name is
followed by a series of constructors preceded by | symbols.  Each constructor
declaration contains the type that it stores:


    type nat =
    | Z
    | S of nat

    type natlist =
    | Nil
    | Cons of nat * natlist

The type language includes user-declared base types (nat), tuples (nat * list),
functions (list -> nat), and the unit type (unit).  Nullary constructors
implicitly store a value of unit type.

Polymorphic datatypes are permitted in a manner identical to OCaml:

    type 'a list =
    | Nil
    | Cons of 'a * 'a list

    type ('a, 'b) list2 =
    | Nil
    | Cons of 'a * 'b * ('a, 'b) list2


## LIBRARY DECLARATIONS
Library declarations introduce names that are available for use during
synthesis.  Every library begins with a let-binding and ends with a double
semicolon (;;).

Library declarations can include constants:

  let x : nat = S (S (S Z)) ;;

Functions:

    let inc (n:nat) : nat = S n ;;

Recursive functions:

    let rec len (ls:list) : nat =
      match ls with
      | Nil u -> Z
      | Cons ls2 -> S (len (#2 ls2))
    ;;

Mutual recursion is unsupported at this time.

All library declarations must have their types annotated and end in double
semicolons.

Inside a library, allowable expressions include match statements:

    match n with
    | Z u  -> ...
    | S n2 -> ...

(*Note*: Nullary patterns must include a variable for the unit value they store)

Tuples:

    (Z, S Z)

Constructors:

    Cons(Z, Nil)
    Z
    S Z

The unit value:

    ()

Function application:

    f (S Z)

Tuple projection:
    #1 t
    #2 (Z, S Z)

(Tuple projection syntax is borrowed from Standard ML - #2 (Z, S Z) --> S Z)

Internal let bindings and function literals are unsupported at this time.

Assuming that standard nat and list types have been declared, standard
natural number and list notation is desugared appropriately:

    type nat =
    | Z
    | S nat

    type list =
    | Nil
    | Cons nat * list

    let seven : nat = 7;;

    let ls : list = [6; 0; 9];;


## SYNTHESIS PROBLEM
At the end of a synthesis input is the synthesis problem itself:

    let function_name : type |> refinement = ?
      OR
    let rec function_name : type |> refinement = ?
      (for recursive functions)

Where:

  * function_name is the name of the function to be synthesized.
  * type is the type of the function (as described in section 10)
  * refinement is a refinement describing the function (see section 13)


## REFINEMENTS
A refinement is kind of type that specifies a smaller set of values than the
standard OCaml type system.  The grammar of refinements is below.  Please
see the paper for a full explanation of how these refinements work.

    r ::=
        | ()                 [unit refinement]
        | C r                [constructor refinement, where C is a constructor]
        | (r, ...)           [tuple refinement]
        | r -> r             [function refinement]
        | (r)                [refinement in parentheses]
        | /\(r, ...)         [intersection refinement]
        | \/(r, ...)         [union refinement]
        | t                  [all types are valid refinements, ie. Cons(nat, list)]
        | i                  [polymorphic constant: any unused identifier is
                              inferred to be a member of the corresponding
                              polymorphic type]
        | not(r)             [negation; r must be a singleton]

As an example, we might specify the decrement function with the refinements:

    let dec : nat -> nat |> /\(\/(0, 1) -> 0, 2 -> 1) = ?

As a polymorphic example, we might specify list map with the refinements:

    let rec map : ('a -> 'b) -> 'a list -> 'b list |>
      (a1 -> b1) -> /\([] -> [], [a1] -> [b1], [a1; a1] -> [b1; b1]) = ?
      
Here, a1 and b1 are polymorphic constants automatically inferred to have
types 'a and 'b respectively.
