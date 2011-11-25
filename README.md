landler
=======

> An extended λ-calculus interpreter

A λ-calculus interpreter
------------------------

At its core, landler is a λ-calculus evaluation library and an
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
