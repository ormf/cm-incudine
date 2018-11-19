(in-package :cm)

(defobject incudine-event (event)
    (fn args)
  #|( :event-streams incudine-file )|#)

(defun simple (&rest args)
  (format t "#'simple invoked with ~a~%" args))

(defun default-incudine-mapper (obj &rest args)
  (declare (ignore args))
  (values #'simple
          (sv obj :time)
          (list
           :keynum (midi-keynum obj)
           :dur (midi-duration obj)
           :amp (midi-amplitude obj)
           :channel (midi-channel obj))))

(defgeneric midi->incudine (obj &rest args))

(defmethod midi->incudine ((obj seq) &rest args)
  (apply #'midi->incudine (subobjects obj) args))

(defmethod midi->incudine ((obj list) &rest args)
  (let ((start (getf args :start 0))
        (end (getf args :end most-positive-fixnum)))
;;    (break "start: ~a end: ~a" start end)
    (mapcar (lambda (midi) (apply #'midi->incudine midi args))
            (remove-if-not (lambda (m) (<= start (sv m :time) end)) obj))))

(defmethod midi->incudine ((obj midi) &rest args)
  (let ((offs (* -1 (getf args :start 0))))
    (multiple-value-bind (fn time args)
        (apply (getf args :dsp-map-fn #'default-incudine-mapper) obj args)
      (new incudine-event :fn fn :time (+ time offs) :args args))))

(defmethod write-event ((obj incudine-event) (stream incudine-stream) scoretime)
  (at (+ (rts-now) scoretime)
      (lambda () (apply (sv obj fn) (sv obj args)))))

(export 'MIDI->INCUDINE 'cm)

(defgeneric midi->sol (obj &rest args))

(defmethod midi->sol ((obj seq) &rest args)
  (apply #'midi->sol (subobjects obj) args))

(defmethod midi->sol ((obj list) &rest args)
  (let ((start (getf args :start 0))
        (end (getf args :end most-positive-fixnum)))
;;    (break "start: ~a end: ~a" start end)
    (mapcar (lambda (midi) (apply #'midi->sol midi args))
            (remove-if-not (lambda (m) (<= start (sv m :time) end)) obj))))

(defmethod midi->sol ((obj midi) &rest args)
  (let ((offs (* -1 (getf args :start 0))))
    (multiple-value-bind (fn time args)
        (apply (getf args :dsp-map-fn #'default-incudine-mapper) obj args)
      (new incudine-event :fn fn :time (+ time offs) :args args))))

(export 'MIDI->SOL 'cm)
