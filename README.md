landler
=======

> An extended λ-calculus interpreter

A λ-calculus interpreter
------------------------

At its core, landler is a λ-calculus evaluation library and a
command-line program that wraps around it.  For instance, you can use
the library interactively with GHCi:

    % ghci
    GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
    Prelude> :m +Language.Landler
    Prelude Language.Landler> dance "(\\x y. y x) World Hello"
    [(((\x y. y x) World) Hello,"subst"),((\y. y World) Hello,"subst"),(Hello World,"stuck")]

The command-line program, `landler` provides a convenient interface to
the library and includes some extra features such as let-bindings:

    % cat > fix.lambda
    let id = (\x. x)
    let fix = (\f. (\x. f (x x)) (\x. f (x x)))
    (fix id)

    % landler fix.lambda
    fix id
	    name-rep
    (\f. (\x. f (x x)) (\x. f (x x))) id
	    subst
    (\x. id (x x)) (\x. id (x x))
	    subst
    id ((\x. id (x x)) (\x. id (x x)))
	    name-rep
    (\x. x) ((\x. id (x x)) (\x. id (x x)))
	    subst
    (\x. id (x x)) (\x. id (x x))
	    cycling
    ---

The `landler` CLI program can also be used as a REPL; just run it
without any arguments:

    % landler
    landler, version <VERSION>: https://github.com/scvalex/landler  :? for help
    ∅ > ((\x. x) y)
    [((\x. x) y,"subst"),(y,"stuck: top-level is not an application")]
    ∅ > type (\x y. x)
    b → c → b

Development
-----------

Development is `Makefile`-driven™.  That is, I run everything through
the `Makefile`.

To build landler, run:

    % make

To build the documentation, run:

    % make doc

To install:

    % make install

To run the tests (both unit and quickcheck):

    % make test

Note that most of these just invoke `cabal` behind the scenes.
