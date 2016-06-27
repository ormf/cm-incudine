(in-package :cm)

;;; forward declarations from incudine-rts.lisp

(declaim (special *fudi-in* *fudi-out*))

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
