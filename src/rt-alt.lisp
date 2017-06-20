;;; **********************************************************************
;;; Copyright (C) 2009 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; generated by scheme->cltl from rt.scm on 15-Mar-2009 20:22:22

(in-package :cm)

(defparameter *pm-not-loaded-error* "recv for portmidi not loaded.")

(defmethod recv ((io portmidi-stream) &key resolution priority) io
           resolution priority (error *pm-not-loaded-error*))

(defmethod recv-stop ((io portmidi-stream)) io
           (error *pm-not-loaded-error*))

(defmethod recv-set! ((io portmidi-stream) hook &key recv-mode)
           recv-mode io hook (error *pm-not-loaded-error*))

(defmethod recv? ((io portmidi-stream)) (error *pm-not-loaded-error*))

(defun rtserr (fn args)
  (error "Attempt to call ~s without RTS loaded." (cons fn args)))

(defun rts (&rest args) (rtserr 'rts args))

(defun rts? (&rest args) args nil)

(defun rts-stop (&rest args) (rtserr 'rts-stop args))

(defun rts-pause (&rest args) (rtserr 'rts-pause args))

(defun rts-continue (&rest args) (rtserr 'rts-continue args))

(defun rts-enqueue (&rest args) (rtserr 'rts-enqueue args))

(defun rts-now (&rest args) (rtserr 'rts-now args))

(defun rts-thread? () nil)
