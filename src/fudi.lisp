(in-package :cm)

(defobject fudi (event)
       ((message :initform 0 :accessor fudi-msg))
     (:parameters time message)
     (:event-streams))

(defun fudi-open (&key (host "127.0.0.1") (port 3001)
                    (protocol :tcp)
                    (direction :input))
  (case direction
    (:output
     (setf *fudi-out* (fudi:open :host host
                                 :port port
                                 :protocol protocol
                                 :direction direction)))
    (t (setf *fudi-in* (fudi:open :host host
                                  :port port
                                  :protocol protocol
                                  :direction :input))))) 

(defun fudi-close (&rest args)
  (if (or (member :inout args)
          (member :input args))
      (setf *fudi-in* (fudi:close *fudi-in*)))
  (if (or (member :inout args)
          (member :output args))
      (setf *fudi-out* (fudi:close *fudi-out*))))

;;; send in incudine implemented as function:

(defun send-fudi (msg)
  "Send a FUDI message."
  (if (fudi:output-stream-p *fudi-out*)
      (fudi:send *fudi-out* msg)))
