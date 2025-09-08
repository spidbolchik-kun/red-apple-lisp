[logo](logo.png)

Install
-----------------------------------------------------
If Gambit is not installed, follow the instructions [here](https://github.com/gambit/gambit).

Install Red Apple Lisp with these commands:

    git clone https://github.com/spidbolchik-kun/red-apple-lisp.git
    cd red-apple-lisp
    ./install.sh

Why another dialect of Lisp?
-------------------

* Custom argument passing semantics with 6 argument groups to make pointfree style easier.
* Infix notation for assignments and destructuring assignments.
* Parameterizable modules instead of typeclasses for generic code.
* Value-level program verification instead of typechecking.
* All values except procedures are mapped to JSON.
* Distinction between structs and dictionaries is invisible to a programmer.
* Purely functional: variable mutations are not allowed, pointer equality does not exist.
* No Lisp-style symbols and chars, only strings.
* Procedural macro system inspired by Clojure with conditional namespace qualification.
* Deep introspection and non-opaque closures.
* Integrated miniKanren: functions are automatically translated to relational form.
* Lazily evaluated: computations are only forced inside IO actions.
* Functional reactive programming primitives for complex state management.
