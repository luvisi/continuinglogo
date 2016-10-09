

ContinuingLogo

ContinuingLogo is a Logo interpreter that is mostly compatible with
Ucblogo.  It successfully runs minimally modified versions of 
Brian Harvey's Pascal compiler and Student program from volume 3 of
Computer Science Logo Style.

It also supports FUNARG's, which are kind of like lexical closures,
but they are closed over the dynamic environment instead of the
lexical environment.  These were in Lisp 1.5, but they have not
been implemented in many recent languages.

For example:

    ? to addx :y
    >   output :x+:y
    > end
    ? to makeclosure
    >   output function [[z] addx :z]
    > end
    ? to makeadder :x
    >   output makeclosure
    > end
    ? make "add5 makeadder 5
    ? print invoke :add5 2
    7
    ?

When makeclosure calls function, it captures the dynamic environment,
including the value of x from makeadder.

First class continuations are supported, as is McCarthy's ambiguous
operator.  For example:

    ? to foo
    >   localmake "x (amb 1 2 3)
    >   localmake "y (amb 4 5 6)
    >   if :x*:y <> 15 [fail]
    >   print :x
    >   print :y
    > end
    ? foo
    3
    5
    ?

The garbage collector is precise, tracing, incremental, and portable.
It knows nothing about the structure of Logo objects.  Callbacks
are used for following the pointers within objects.

INSTALLATION

ContinuingLogo uses autoconf.  With any luck, the following may work:

    $ ./configure; make; make install

RUNNING

Normal usage:

    $ clogo

EXAMPLES

To run the pascal compiler:

    $ cd ucblogo/
    $ rlwrap clogo
    ? load "pascal
    Loaded
    ? compile "tower
    <printout of pascal program as it is compiled>
    ? prun "tower
    <moves to solve the Tower of Hanoi>

To run the student example:

    $ cd ucblogo/
    $ rlwrap clogo
    ? load "student
    Loaded
    ? student :radio
    <printout of the solution of a mathematical word problem>

LICENSE

Most of the interpreter is released under version 3 or later of the GNU
General Public License.

The Logo library procedures in ucblogolib.txt are from Ucblogo and
are covered by version 2 or later of the GNU General Public License.

The garbage collector is released under version 2.0 of the Apache
License.

LIMITATIONS AND INCOMPATIBILITIES COMPARED TO UCBLOGO

* The reader and the treeifier use recursive descent parsers written in
  C.  They can be crashed with pathologically long or deep structures.

* There is no support for getter/setter notation, or GLOBAL.

* No TEST/IFTRUE/IFFALSE.

* No TAG/GOTO.

* Slot number variables only go up to ?10.

* There is no RUNPARSE.  Lists are only parsed once.  [a+b] is a list with
  three items.

* Backslashed characters do not keep any special properties.  There is no
  BACKSLASHEDP.

* SETITEM behaves the same as .SETITEM behaves.  It does not check for cycles.

* LOCAL will not work inside of templates run with APPLY, INVOKE, FOR, etc.

* There is no PENREVERSE.

* There is no PALETTE/SETPALETTE.  Color commands take rgb lists, with
  each value ranging from 0-255.

* There is no SETPENPATTERN/PENPATTERN.  (These are NOPs on several ucblogo
  platforms as well.)

* There is no SETSCRUNCH/SCRUNCH.

* SAVEPICT/LOADPICT use PNG format.  There is no EPSPICT.
