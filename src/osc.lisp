(in-package :cm)

(defobject osc (event)
       ((path :initform "/osc" :accessor osc-path)
        (types :initform "i" :accessor osc-types)
        (message :initform 0 :accessor osc-msg))
     (:parameters time path message)
     (:event-streams))


(defun osc-open (&key (host "127.0.0.1") (port 3001)
                    (protocol :tcp)
                    (direction :input))
  (case direction
    (:output
     (progn
       (setf *osc-out* (osc:open :host host
                                 :port port
                                 :protocol protocol
                                 :direction direction))
       (setf (osc:broadcast *osc-out*) t)
       *osc-out*))
    (t (setf *osc-in* (osc:open :host host
                                :port port
                                :protocol protocol
                                :direction :input)))))

(defun osc-close (&rest args)
  (if (or (not args)
          (member :inout args)
          (member :input args))
      (if *osc-in*
          (progn
            (osc:close *osc-in*)
            (setf *osc-in* nil))))
  (if (or (not args)
          (member :inout args)
          (member :output args))
      (if *osc-out*
          (progn
            (osc:close *osc-out*)
            (setf *osc-out* nil)))))

;;; send in incudine implemented as function:

(defun send-osc (address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES
to *osc-out*."
  (let ((stream *osc-out*))
    (if stream
        (progn
          (osc:start-message stream address types)
          (loop for val in values for i from 0
             do (osc::set-value stream i val))
          (osc:send stream)))))

(defun osc::send-osc (stream address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES
To a specifiable stream."
  (osc:start-message stream address types)
  (loop for val in values for i from 0
     do (osc::set-value stream i val))
  (osc:send stream))

(defmacro message2 (stream address types values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES."
  `(progn
     (osc:start-message ,stream ,address ,types)
     ,@(loop for val in values for i from 0
             collect `(osc::set-value ,stream ,i ,val))
     (osc:send ,stream)))
