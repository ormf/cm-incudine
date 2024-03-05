;;; rt.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cm)

#|
(defparameter *pm-not-loaded-error* "recv for portmidi not loaded.")

(defmethod recv ((io portmidi-stream) &key resolution priority) io
           resolution priority (error *pm-not-loaded-error*))

(defmethod recv-stop ((io portmidi-stream)) io
           (error *pm-not-loaded-error*))

(defmethod recv-set! ((io portmidi-stream) hook &key recv-mode)
           recv-mode io hook (error *pm-not-loaded-error*))

(defmethod recv? ((io portmidi-stream)) (error *pm-not-loaded-error*))
|#

(defparameter *time-format* :sec)
(defparameter *cm-rts-started* nil)

(defun set-time-format (fmt)
  (if (member fmt '(:sec :sample :ms))
      (setf *time-format* fmt)
      (error "time-format ~a not supported, must be :sec :sample or :ms!" fmt)))

(defun rts-now ()
  (case *time-format*
    ((:sec) (* (incudine:now) incudine::*sample-duration*))
    ((:sample) (incudine:now))
    ((:ms) (* (incudine:now) incudine::*sample-duration* 1000))))

(defun rts (&key (rt-wait 0))
;;;  (cm)
  (cl-midictl:start-midi-engine)
  (sleep rt-wait)
  (if *midi-out1*
      (setf *rts-out* *midi-out1*)
      (error "couldn't assign *rts-out* (increase rt-wait)"))
  (incudine:rt-start)
  (setf *cm-rts-started* t)
  :cm-rts-started)

(defun rts? (&optional arg)
  (declare (ignore arg))
  (and (eq :started (incudine:rt-status))
       *cm-rts-started*))

(defparameter *rts-thread* nil)

(defun rts-thread? ()
  *rts-thread*)

#|
(defun rts-thread? ()
  (if incudine::*rt-thread* T))
|#

(defun rtserr (fn args)
  (error "Attempt to call ~s without RTS loaded." (cons fn args)))

(defun rts-stop (&rest args) (rtserr 'rts-stop args))

(defun rts-pause (&rest args) (rtserr 'rts-pause args))

(defun rts-continue (&rest args) (rtserr 'rts-continue args))

;;; (defun rts-enqueue (&rest args) (rtserr 'rts-enqueue args))

(defun rts-enqueue (handle object time start sched)
  ;; time is either in sec msec or usec
  ;; sched is either :rts or nil, nil means from repl
  ;; add check for sprout without rts running.
  (declare (ignore sched))
  (let (
;;;        (repl? (not (eql sched ':rts)))
	(data 0)
;;;	(flag 0)
        )
    (cond ((= (logand handle #xF) *qentry-message*)
	   ;; integer midi messages can be inserted directly into the
	   ;; scheduler as data. could do C pointers this way too.
	   (setq data object))
	  ((= 0 (logandc2 handle #xF)) ; handle is less than 10000 (unsigned)
           ;; new entry, add to table
	   ;; if its a seq or a process we also have to cache the
	   ;; start time of the object: (<object> . start)
	   ;; start time is in *time-format* units 
           (cond ((= handle *qentry-process*)
;;;                    (format t "~2&rts-enqueue: time: ~10,0f, start: ~10,0f~%" time start)
                  (at time
                      (lambda ()
                        (let ((*rts-thread* t))
                          (scheduler-do-process object
                                                time
                                                start
                                                *rts-out*
                                                handle
                                                ':rts)))))
                 ((= handle *qentry-seq*)
                  (at time
                      (lambda ()
                        (let ((*rts-thread* t))
                          (scheduler-do-seq object
                                            time
                                            start
                                            *rts-out*
                                            handle
                                            ':rts)))))
                 ((= handle *qentry-object*)
                  (at time
                      (lambda () (output object :to *rts-out*)))))))

    #+(or)(unless (eql flag 0)
            (case flag
              ((1) (error "enqueue: no room in scheduler for ~S." object))
              ((2) (error "enqueue: RTS not running."))))
    (values)))
