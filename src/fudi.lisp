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
                            (host "127.0.0.1")
                            (port 3001)
                            (protocol :tcp)
                            (element-type 'character)
                            (direction :input))
  (case direction
    (:output
     (progn
       (fudi-close-default :output)
       (setf *fudi-out* (fudi:open :host host
                                   :port port
                                   :protocol protocol
                                   :element-type element-type
                                   :direction :output))))
    (t (progn
         (fudi-close-default :input)
         (setf *fudi-in* (fudi:open :host host
                                    :port port
                                    :protocol protocol
                                    :element-type element-type
                                    :direction :input))
         (fudi::start-socket-server *fudi-in*)))))

(defun fudi-open (&key
                    (host "127.0.0.1") (port 3001)
                    (protocol :tcp)
                    (element-type 'character)
                    (direction :input))
  (fudi:open :host host
             :port port
             :protocol protocol
             :element-type element-type
             :direction direction))


(defun fudi-close-default (&rest args)
  (if (or (member :inout args)
          (member :input args)
          (not args))
      (if (and *fudi-in* (slot-value *fudi-in* 'fudi::socket))
          (progn
            (fudi:close *fudi-in*)
            (setf *fudi-in* nil))))
  (if (or (member :inout args)
          (member :output args)
          (not args))
      (if (and *fudi-out* (slot-value *fudi-out* 'fudi::socket))
          (progn
            (fudi:close *fudi-out*)
            (setf *fudi-out* nil)))))

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


;;; dummy method to make set-receiver! happy
(defmethod rt-stream-receive-type ((stream fudi:input-stream))
  *midi-rcv-type-dummy*)

;;; dummy method to make set-receiver! happy
(defmethod object-name ((stream fudi:input-stream))
  *midi-obj-name-dummy*)

(defmethod rt-stream-receive-data ((stream fudi:input-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-receive-init ((stream fudi:input-stream) hook args)
  (if (gethash stream *stream-recv-responders*)
      (progn
        (incudine:remove-responder (gethash stream *stream-recv-responders*))
        (remhash stream *stream-recv-responders*)))
  (let* ((responder
          (incudine::make-fudi-responder
           stream hook)))
    (if responder
        (setf (gethash stream *stream-recv-responders*) responder)
        (error "~a: Couldn't add responder!" stream)))
  (values))

(defmethod stream-receive-start ((stream fudi:input-stream) args)
  args
  (if (incudine::receiver-status
           (incudine::receiver stream))
      T
      (incudine:recv-start stream)))

(defmethod stream-receive-stop ((stream fudi:input-stream))
    (incudine:recv-stop stream)
  (values))

(defmethod stream-receive-deinit ((stream fudi:input-stream))
  (if (incudine:remove-responder (gethash stream *stream-recv-responders*))
      (error "~a: Couldn't remove responder!" stream)
      (remhash stream *stream-recv-responders*)))

(export '(*fudi-in* *fudi-out* fudi) 'cm)
