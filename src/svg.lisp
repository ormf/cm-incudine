(in-package :cm)

;;; forward declarations from incudine-rts.lisp

(declaim (special *fudi-in* *fudi-out*))

(progn
 (defclass svg-file (event-file)
           ((parts :initarg :parts :initform '() :accessor
             fomus-file-parts)
            (global :initarg :global :initform '() :accessor
             fomus-file-global)
            (view :initarg :view :initform t :accessor
             fomus-file-view)
            (play :initarg :play :initform nil :accessor
             fomus-file-play)
            (tempo :initarg :tempo :initform 60 :accessor
             fomus-file-tempo))
           #+metaclasses  (:metaclass io-class))
 (defparameter <svg-file> (find-class 'svg-file))
 (finalize-class <svg-file>)
 (setf (io-class-file-types <svg-file>) '("*.svg"))
 (values))

(defobject svg-line (event)
    ((message :initform 0 :accessor fudi-msg)
     (stream :initform *fudi-out* :accessor fudi-stream))
     (:parameters time message stream)
     (:event-streams))

(defmethod write-event ((obj midi) (mf svg-file) time)
           (let ((beats time)
                 (scaler (midi-file-scaler mf))
                 (keyn (midi-keynum obj))
                 (chan (midi-channel obj))
                 (ampl (midi-amplitude obj))
                 (last nil))
             (ensure-velocity ampl keyn)
             (ensure-microtuning keyn chan mf)
             (unless (< keyn 0)
               (setf last
                       (if (null (%q-head %offs)) (object-time mf)
                           (flush-pending-offs mf beats)))
               (midi-write-message (make-note-on chan keyn ampl) mf
                (if (> beats last) (round (* (- beats last) scaler))
                    0)
                nil)
               (setf (object-time mf) beats)
               (%q-insert
                (%qe-alloc %offs (+ beats (midi-duration obj)) nil
                 (make-note-off chan keyn 127) *qentry-message*)
                %offs))
             (values)))
