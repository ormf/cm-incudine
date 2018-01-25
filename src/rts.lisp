;;; rts.lisp
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

(defun samps->time (samps)
  (case *time-format*
    ((:sec) (* samps incudine.util:*sample-duration*))
    ((:sample) samps)
    ((:ms) (* samps incudine.util:*sample-duration* 1000))))

(defun time->samps (time)
  (case *time-format*
    ((:sec) (* time incudine.util:*sample-rate*))
    ((:sample) time)
    ((:ms) (* time incudine.util:*sample-rate* 0.001))))

(defun time->ms (time)
  (case *time-format*
    ((:sec) (* time 1000.0))
    ((:sample) (* time incudine.util:*sample-duration* 1000))
    ((:ms) time)))

(defun secs->samps (secs)
  (* secs incudine.util:*sample-rate*))

(defun samps->secs (samps)
  (* samps incudine.util:*sample-duration*))

(defun at (time function &rest args)
  (apply #'incudine:at (time->samps time) function args))

(defun amp->velo (amp)
  (round (* 127 (min 1 (max 0 amp)))))

;;; dummy method to make set-receiver! happy
(define-setf-expander rt-stream-receive-type (stream)
  (declare (ignore stream))
  nil)

;;; we need to store the responder for an input stream type globally for
;;; stream-receive-deinit
(defvar *stream-recv-responders* (make-hash-table))

(alexandria:define-constant +ml-opcode-mask+ #b11110000)
(alexandria:define-constant +ml-channel-mask+ #b1111)

(defun set-receiver! (hook stream &rest args)
  (let ((data (rt-stream-receive-data stream)))
    (if (and (not (null data)) (first data))
        (error "set-receiver!: ~s already receiving." stream)
        (let ((type (getf args ':receive-type)))
          (if type (setf (rt-stream-receive-type stream) type)
              (if (not (rt-stream-receive-type stream))
                  (setf (rt-stream-receive-type stream)
                          *receive-type*)))
          (stream-receive-init stream hook args)
          (cond
           ((stream-receive-start stream args)
            (format t "~%; ~a receiving!" (object-name stream)))
           (t (stream-receive-deinit stream)
            (error
             "set-receiver!: ~s does not support :receive-type ~s."
             stream (rt-stream-receive-type stream))))
          (values)))))



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

(defun rts ()
  (unless *rts-out* (setf *rts-out* (new incudine-stream)))
  (incudine:rt-start)
  (midi-open-default :direction :input)
  (midi-open-default :direction :output))

(export '(*rts-out*
          samps->time time->samps secs->samps samps->secs at amp->velo
          write-event set-receiver! rts-enqueue rts)
        :cm)
