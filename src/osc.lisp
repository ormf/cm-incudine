;;; osc.lisp
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

(defvar *osc-in* nil)
(defvar *osc-out* nil)

(defun osc-output-stream ()
  *osc-out*)

(defobject osc (event)
       ((path :initform "/osc" :accessor osc-path)
        (types :initform "i" :accessor osc-types)
        (message :initform 0 :accessor osc-msg)
        (stream :initform *osc-out* :accessor osc-stream))
     (:parameters time path message)
     (:event-streams))

(defun osc-open-default (&key (host "127.0.0.1") (port 3001)
                           (protocol :udp)
                           (direction :input))
  (case direction
    (:output
     (progn
       (if *osc-out* (osc-close-default))
       (setf *osc-out* (incudine.osc:open :host host
                                          :port port
                                          :protocol protocol
                                          :direction direction))
       (setf (incudine.osc:broadcast *osc-out*) t)
       *osc-out*))
    (t (progn
         (if *osc-in* (osc-close-default))
         (setf *osc-in* (incudine.osc:open :host host
                                  :port port
                                  :protocol protocol
                                  :direction :input))))))

(defun osc-close-default (&rest args)
  (if (or (not args)
          (member :inout args)
          (member :input args))
      (if *osc-in*
          (progn
            (incudine.osc:close *osc-in*)
            (setf *osc-in* nil))))
  (if (or (not args)
          (member :inout args)
          (member :output args))
      (if *osc-out*
          (progn
            (incudine.osc:close *osc-out*)
            (setf *osc-out* nil)))))

(defmacro osc-close (stream)
  `(if (and ,stream (incudine.osc:stream ,stream))
       (progn
         (incudine.osc:close ,stream)
         (setf ,stream nil))))

;;; send in incudine implemented as function:

(defun send-osc (address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES
to *osc-out*."
  (let ((stream *osc-out*))
    (if stream
        (progn
          (incudine.osc:start-message stream address types)
          (loop for val in values for i from 0
             do (incudine.osc::set-value stream i val))
          (incudine.osc:send stream)))))

(defun incudine.osc::send-osc (stream address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES
To a specifiable stream."
  (incudine.osc:start-message stream address types)
  (loop for val in values for i from 0
     do (incudine.osc::set-value stream i val))
  (incudine.osc:send stream))

(defmacro message2 (stream address types values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES."
  `(progn
     (incudine.osc:start-message ,stream ,address ,types)
     ,@(loop for val in values for i from 0
             collect `(incudine.osc::set-value ,stream ,i ,val))
     (incudine.osc:send ,stream)))



(defmethod write-event ((obj osc) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (osc-output-stream))
;;    (format t "scoretime: ~a~%" scoretime)
    (at (+ (rts-now) scoretime)
                 (lambda () (apply #'incudine.osc::send-osc stream (osc-path obj)
                              (osc-types obj)
                              (let ((msg (osc-msg obj)))
                                (if (consp msg) msg (list msg)))))))
  (values))
