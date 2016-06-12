(in-package :cm)

(defobject osc (event)
       ((path :initform "/osc" :accessor osc-path)
        (types :initform "i" :accessor osc-types)
        (message :initform 0 :accessor osc-msg))
     (:parameters time path message)
     (:event-streams))

;;; send in incudine implemented as function:

(defun send-osc (stream address types &rest values)
  "Send a OSC message with OSC ADDRESS, OSC TYPES and arbitrary VALUES."
  (osc:start-message stream address types)
  (loop for val in values for i from 0
     do (osc::set-value stream i val))
  (osc:send stream))
