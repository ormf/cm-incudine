"cm-incudine" extends common music 2 (the common lisp port of common
music) with realtime capabilities using the "incudine" realtime system
by Tito Latini. Currently realtime midi, FUDI and any function call
are supported. Note: incudine currently only works with sbcl.

The package depends on

- cm
- incudine
- quicklisp (or asdf)
- fudi-incudine

As all the code is evaluated within the "cm" package and exported from
there, code evaluation should take place within the "cm" package as
well.

Realtime processes can be invoked by either using the events function
to an <incudine-stream> or the sprout function. The processes now also
supports realtime fudi messages in addition to osc and midi
messages. To get this done first get the prerequisites:

- [incudine](http://incudine.sourceforge.net/)
- [fudi-incudine](https://github.com/ormf/fudi-incudine)
- [cm](https://github.com/ormf/cm)

Using a common lisp with quicklisp installed, copy all the folders of
the prerequsites into the local-projects subfolder of your local
quicklisp installation folder (default is ~/quicklisp/local-projects).

Then start sbcl and evaluate the following:

```cl
(ql:quickload "cm-incudine")

(progn
  (incudine:rt-start)
  (sleep 1)
  (midi-open-default :direction :input)
  (midi-open-default :direction :output)
  (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
  (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)
  (fudi-open-default :host "127.0.0.1" :port 3011 :direction :input)
  (fudi-open-default :host "127.0.0.1" :port 3012 :direction :output)
  (setf *out* (new incudine-stream))
  (setf *rts-out* *out*))
```
Now you can send realtime midi messages to jackmidi output, tcp
messages to pd listening on port 3003, or osc messages with udp to
port 3012 like this:
```cl
(events (new midi :time 0 :keynum 60 :duration 1) *rts-out*)

(sprout
   (process
     repeat 2
     output (new fudi :time (now) :message '(1 2 3 4))
     wait 1) :at (now))

(sprout
   (process
     repeat 2
     output (new osc :time (now) :types "iiii" :message '(1 2 3 4))
     wait 1) :at (now))
```
The default time format is :sec but can also be set to :sample or :ms
with the function #'set-time-format

The functions #'at and #'now are wrappers for the same incudine
functions which automatically translate from/to the current
time-format.

Receiving from fudi, osc and midi streams also works. Please refer to
incudine's documentation for information how to set up receivers.

For other usage examples see the file "src/cm-incudine-examples.lisp".

Orm Finnendahl 2017
