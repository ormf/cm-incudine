(in-package :cm)

(defobject osc (event)
       ((path :initform "/osc" :accessor osc-path)
        (types :initform "i" :accessor osc-types)
        (message :initform 0 :accessor osc-msg))
     (:parameters time path message)
     (:event-streams))
