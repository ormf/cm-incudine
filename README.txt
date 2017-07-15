"cm-incudine" extends common music 2 (the common lisp port of common
music) with realtime capabilities using the incudine realtime system
by Tito Latini. Currently realtime midi, FUDI and any function call
are supported.

The package depends on

- cm
- incudine

As all the code is evaluated within the "cm" package and exported from
there, code evaluation should take place within the "cm" package as
well.

For information about the usage see the file
"src/cm-incudine-examples.lisp".

Orm Finnendahl 2017
