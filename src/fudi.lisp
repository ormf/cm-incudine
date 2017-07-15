;;; fudi.lisp
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

(defparameter *fudi-in* nil)
(defparameter *fudi-out* nil)

(defobject fudi (event)
    ((message :initform 0 :accessor fudi-msg)
     (stream :initform *fudi-out* :accessor fudi-stream))
     (:parameters time message stream)
     (:event-streams))

(defun fudi-open-default (&key
                            (host "127.0.0.1") (port 3001)
                            (protocol :tcp)
                            (direction :input))
  (case direction
    (:output
     (progn
       (fudi-close-default :output)
       (setf *fudi-out* (fudi:open :host host
                                   :port port
                                   :protocol protocol
                                   :direction direction))))
    (t (progn
         (fudi-close-default :input)
         (setf *fudi-in* (fudi:open :host host
                                         :port port
                                         :protocol protocol
                                         :direction :input)))))) 

(defun fudi-open (&key
                    (host "127.0.0.1") (port 3001)
                    (protocol :tcp)
                    (direction :input))
  (fudi:open :host host
             :port port
             :protocol protocol
             :direction direction))


(defun fudi-close-default (&rest args)
  (if (or (member :inout args)
          (member :input args)
          (not args))
      (progn
        (fudi:close *fudi-in*)
        (setf *fudi-in* nil)))
  (if (or (member :inout args)
          (member :output args)
          (not args))
      (progn
        (fudi:close *fudi-out*)
        (setf *fudi-out* nil))))

(defmacro fudi-close (stream)
  `(if (and ,stream (fudi:stream-p ,stream))
       (progn
         (fudi:close ,stream)
         (setf ,stream nil))))

;;; send in incudine implemented as function:

(defun send-fudi (msg &key (stream *fudi-out*))
  "Send a FUDI message."
  (if (fudi:output-stream-p stream)
      (fudi:send stream msg)
      (warn "No valid output-stream: ~a" stream)))




(defmethod write-event ((obj fudi) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (fudi-stream obj))
    (at (+ (rts-now) scoretime)
                 (lambda () (funcall #'send-fudi
                                (let ((msg (fudi-msg obj)))
                                  (if (consp msg) msg (list msg)))
                                :stream stream))))
  (values))

(defun fudi-output-stream ()
  *fudi-out*)

(export '(*fudi.in* *fudi-out* fudi fudi-open-default fudi-open fudi-open-default fudi-close-default
           send-fudi fudi-output-stream) :cm)
