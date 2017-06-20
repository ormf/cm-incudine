(in-package :cm)

#|

;;; todo: Adapt the following definitions to incudine's
;;; streaming protocol to get full integration into cm's midi
;;; capabilities (like channel-tuning, process integration 
;;; with special clauses, etc.).
;;; 
;;; This is distinct from incudine-rts which aims to be able to
;;; mix midi, fudi and functions in the same rt-process. 
;;;
;;; 
;;; It needs checking whether this could be integrated into
;;; incudine-rts as well. Otherwise jackmidi-stream could serve as
;;; a standalone protocol for midi only rt-streams.
;;;
;;; Use pm.lisp as reference.

(defparameter *jackmidi-default-input* nil)
(defparameter *jackmidi-default-output* nil)
(defparameter *jackmidi-default-latency* 5)
(defparameter *jackmidi-default-inbuf-size* 512)
(defparameter *jackmidi-default-outbuf-size* 2048)
(defparameter *jackmidi-default-filter* 0)
(defparameter *jackmidi-default-mask* 0)

(progn
 (defclass jackmidi-stream (rt-stream midi-stream-mixin)
           ((input :initform *jackmidi-default-input* :initarg :input
             :accessor jackmidi-input)
            (output :initform *jackmidi-default-output* :initarg
             :output :accessor jackmidi-output)
            (latency :initform *jackmidi-default-latency* :initarg
             :latency :accessor rt-stream-latency)
            (inbufsize :initform *jackmidi-default-inbuf-size*
             :initarg :inbuf-size :accessor jackmidi-inbuf-size)
            (outbufsize :initform *jackmidi-default-outbuf-size*
             :initarg :outbuf-size :accessor jackmidi-outbuf-size)
            (receive-data :initform (list nil nil nil nil) :accessor
             rt-stream-receive-data)
            (receive-mode :initform :message :initarg :receive-mode
             :accessor rt-stream-receive-mode)
            (filter :initform *jackmidi-default-filter* :initarg
             :filter :accessor jackmidi-filter)
            (mask :initform *jackmidi-default-mask* :initarg
             :channel-mask :accessor jackmidi-channel-mask)
            (offset :initform 0 :initarg :offset :accessor
             jackmidi-offset))
           #+metaclasses  (:metaclass io-class))
 (defparameter <jackmidi-stream> (find-class 'jackmidi-stream))
 (finalize-class <jackmidi-stream>)
;;; (setf (io-class-file-types <jackmidi-stream>) '("*.ic"))
 (values))


(defmethod open-io ((obj jackmidi-stream) dir &rest args)
  (declare (ignore dir args))
;;;  (format t "open-io: ~a, io-open: ~a" obj (io-open obj))
           (when (not (io-open obj)))
           obj)

(defmethod close-io ((obj jackmidi-stream) &rest mode)
  (declare (ignore obj mode))
  (values))

|#
