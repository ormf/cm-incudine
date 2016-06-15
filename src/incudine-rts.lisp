(in-package :cm)

(defparameter *incudine-default-input* nil)

(defparameter *incudine-default-output* nil)

(defparameter *incudine-default-latency* 5)

(defparameter *incudine-default-inbuf-size* 512)

(defparameter *incudine-default-outbuf-size* 2048)

(defparameter *incudine-default-filter* 0)

(defparameter *incudine-default-mask* 0)


(progn
 (defclass incudine-stream (rt-stream midi-stream-mixin)
           ((input :initform *incudine-default-input* :initarg :input
             :accessor incudine-input)
            (output :initform *incudine-default-output* :initarg
             :output :accessor incudine-output)
            (latency :initform *incudine-default-latency* :initarg
             :latency :accessor rt-stream-latency)
            (inbufsize :initform *incudine-default-inbuf-size*
             :initarg :inbuf-size :accessor incudine-inbuf-size)
            (outbufsize :initform *incudine-default-outbuf-size*
             :initarg :outbuf-size :accessor incudine-outbuf-size)
            (receive-data :initform (list nil nil nil nil) :accessor
             rt-stream-receive-data)
            (receive-mode :initform :message :initarg :receive-mode
             :accessor rt-stream-receive-mode)
            (filter :initform *incudine-default-filter* :initarg
             :filter :accessor incudine-filter)
            (mask :initform *incudine-default-mask* :initarg
             :channel-mask :accessor incudine-channel-mask)
            (offset :initform 0 :initarg :offset :accessor
             incudine-offset))
           #+metaclasses  (:metaclass io-class))
 (defparameter <incudine-stream> (find-class 'incudine-stream))
 (finalize-class <incudine-stream>)
 (setf (io-class-file-types <incudine-stream>) '("*.ic"))
 (values))

(defmethod print-object ((obj incudine-stream) port)
           (let ((name (object-name obj))
                 (pids (event-stream-stream obj))
                 (*print-case* ':downcase))
             (setf name
                     (if name
                         (format nil "~a \"~a\""
                                 (class-name (class-of obj)) name)
                         (format nil "~a"
                                 (class-name (class-of obj)))))
             (if pids
                 (if (car pids)
                     (if (cadr pids)
                         (format port "#<~a (in:~d out:~d)>" name
                                 (car pids) (cadr pids))
                         (format port "#<~a (in:~d)>" name
                                 (car pids)))
                     (if (cadr pids)
                         (format port "#<~a (out:~d)>" name
                                 (cadr pids))
                         (format port "#<~a>" name)))
                 (format port "#<~a>" name))))

(defmethod open-io ((obj incudine-stream) dir &rest args)
  (declare (ignore dir args))
;;;  (format t "open-io: ~a, io-open: ~a" obj (io-open obj))
           (when (not (io-open obj)))
           obj)

(defmethod close-io ((obj incudine-stream) &rest mode)
  (declare (ignore obj mode))
           (values))

(defmethod initialize-io ((obj incudine-stream))
  (channel-tuning-init obj))

(defun incudine-open (&rest args)
  (apply #'open-io "incudine-rts.ic" t args))

(defun incudine-open? (&optional (port (find-object "incudine-rts.ic")))
  (if port
      (let ((io (io-open port)))
        (if io
            (if (car io) (if (cadr io) :inout :in)
                (if (cadr io) :out nil))
            nil))
      nil))

(defun incudine-close (&optional (port (find-object "incudine-rts.ic")))
  (if (incudine-open? port)
      (progn
       (if (equal ':running (recv? port)) (recv-stop port))
       (close-io port ':force))
      port)
  port)

(defun amp->velo (amp)
  (round (* 127 (min 1 (max 0 amp)))))

(defun jackmidi-input-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*input-streams* 0)))

(defun jackmidi-output-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*output-streams* 0)))

(declaim (special *oscout*))

(defun osc-output-stream ()
  *oscout*)

(declaim (inline midi-out))
(defun midi-out (stream status data1 data2 data-size)
  "create a closure to defer a call to jm_write_event
out."
  (lambda ()
    (jackmidi:write-short stream (jackmidi:message status data1 data2) data-size)))

(declaim (inline ctl-out))
(defun ctl-out (stream ccno ccval chan)
  "wrapper for midi ctl-change messages."
  (let ((status (+ chan (ash #b1011 4))))
    (midi-out stream status ccno ccval 3)))

;;; (defparameter evt1 (ctl-out 1 17 1))
;;; (rtevt evt1)

(declaim (inline note-on))
(defun note-on (stream pitch velo chan)
  "wrapper for midi note-on messages."
  (let ((status (+ chan (ash #b1001 4))))
    (midi-out stream status pitch velo 3)))

(declaim (inline note-off))
(defun note-off (stream pitch velo chan)
  "wrapper for midi note-on messages."
  (let ((status (+ chan (ash #b1000 4))))
    (midi-out stream status pitch velo 3)))

(declaim (inline pitch-bend))
(defun pitch-bend (stream bendval chan)
  "wrapper for midi pitchbend messages (range between -1 and 1!)"
  (let* ((status (+ chan (ash #b1000 4)))
         (bval (round (* (1+ bendval) 8191.5)))
         (low (logand bval #x7f))
         (high (ash (logand bval #x3f80) -7)))
    (midi-out stream status low high 3)))

(declaim (inline pgm-change))
(defun pgm-change (stream pgm chan)
  "wrapper for midi program-change messages."
  (let* ((status (+ chan (ash #b1100 4))))
    (midi-out stream status pgm 0 2)))

(declaim (inline midi-note))
(defun midi-note (stream time pitch dur velo chan)
  (incudine:at time (note-on stream pitch velo chan))
  (incudine:at (+ time (* incudine::*sample-rate* dur)) (note-off stream pitch 0 chan)))

(defun pm-message->midi-message (pmm)
  (declare (ignore pmm))
  ;; (let ((status (incudine:message.status pmm)))
  ;;   (if (< status 240)
  ;;       (if (logtest status 128)
  ;;           (let ((stat (ash (logand status 240) -4))
  ;;                 (dat2 (incudine:message.data2 pmm)))
  ;;             (when (and (= stat +ml-note-on-opcode+) (= 0 dat2))
  ;;               (setf stat +ml-note-off-opcode+)
  ;;               (setf dat2 127))
  ;;             (make-channel-message stat (logand status 15)
  ;;              (incudine:message.data1 pmm) dat2))
  ;;           (error "pm-message->midi-message: running status :("))
  ;;       (if (= status 255)
  ;;           (error "pm-message->midi-message: meta message??!")
  ;;           (let ((type (logand status 15)))
  ;;             (if (= type 0)
  ;;                 (error "pm-message->midi-message: sysex :(")
  ;;                 (make-system-message (logior (ash type 4) 15) 0
  ;;                  (incudine:message.data1 pmm)
  ;;                  (incudine:message.data2 pmm)))))))
  )

(defun midi-message->pm-message (mm)
  (declare (ignore mm))
  ;; (incudine:message
  ;;  (logior (ash (midimsg-upper-status mm) 4)
  ;;          (midimsg-lower-status mm))
  ;;  (channel-message-data1 mm) (channel-message-data2 mm))
  )

(defmethod midi-write-message
           ((obj integer) (str incudine-stream) scoretime data)
  (declare (ignore obj str scoretime data))
  ;; (cond ((sysex-p obj))
           ;;       ((or (channel-message-p obj) (system-message-p obj))
           ;;        (incudine:writeshort (second (io-open str))
           ;;                             (if
           ;;                              (scheduling-mode? ':events)
           ;;                              (+ (round (* scoretime 1000))
           ;;                                 (incudine-offset str))
           ;;                              (incudine:time))
           ;;                             (midi-message->pm-message
           ;;                              obj))))
           )

(defun incudine-ensure-microtuning (keyn chan stream incudine-stream time)
  (declare (type (or fixnum single-float symbol) keyn)
           (type (integer 0 15) chan)
           (type jackmidi:output-stream stream)
           (type incudine.util:sample time)
           (optimize speed))
  (flet ((truncate-float (k &optional round-p)
           (declare (type (single-float -2e2 2e4) k))
           (if round-p (round k) (floor k))))
    (let ((num 0)
          (rem 0.0)
          (dat nil))
      (declare (type fixnum num) (type single-float rem) (type list dat))
      (cond ((integerp keyn) nil)
            ((and keyn (symbolp keyn))
             (setf keyn (keynum keyn)))
            ((numberp keyn)
             (setf dat (midi-stream-tunedata incudine-stream))
             (cond ((null dat)
                    (setf keyn (truncate-float keyn t)))
                   ((eq (first dat) t)
                    (setf num (second dat))
                    (let ((int (truncate-float keyn)))
                      (setf rem (- keyn int))
                      (setf keyn int))
                    (if (and *midi-skip-drum-channel*
                             (= (+ (the fixnum (fourth dat)) num) 8))
                        (incf num))
                    (setf num (if (< num (the fixnum (third dat))) (1+ num) 0))
                    (rplaca (cdr dat) num)
                    (setf chan (+ (the fixnum (fourth dat)) num))
                    (let* ((width (or (fifth dat) 2))
                           (bend (truncate-float
                                  (rescale rem (- width) width 0 16383) t)))
                      (declare (type (signed-byte 16) width bend))
                      (incudine:at time
                        (midi-out stream
                          (logior #.(ash +ml-pitch-bend-opcode+ 4) chan)
                          (ldb (byte 7 0) bend) (ldb (byte 7 7) bend) 3))))
                   (t (setf num (second dat))
                      (let* ((qkey (quantize keyn (/ 1.0 num)))
                             (int (truncate-float qkey)))
                        (declare (type single-float qkey) (type fixnum int))
                        (setf rem (- qkey int))
                        (setf keyn int))
                      (setf chan (+ (the fixnum (first dat))
                                    (truncate-float (* rem num)))))))
            (t (error "midi keynum ~s not key number or note." keyn)))
      (values keyn chan))))

(declaim (inline incudine-ensure-velocity))
(defun incudine-ensure-velocity (keyn ampl)
  (declare (type (or (integer 0 127) (single-float 0.0 1.0)) ampl))
  (cond ((floatp ampl)
         (if (= ampl 0.0)
             (values -1 0)
             (values keyn (floor (* ampl 127)))))
        (t (if (= ampl 0)
               (values -1 0)
               (values keyn ampl)))))

(defmethod write-event ((obj midi) (str incudine-stream) scoretime)
  (declare (type (or fixnum single-float) scoretime))
  (alexandria:if-let (stream (jackmidi-output-stream))
;;    (format t "~a~%" scoretime)
    (multiple-value-bind (keyn ampl)
        (incudine-ensure-velocity (midi-keynum obj) (midi-amplitude obj))
      (declare (type (integer 0 127) ampl))
      (let ((time (+ (incudine:now) (* incudine.util:*sample-rate* scoretime))))
        (multiple-value-bind (keyn chan)
            (incudine-ensure-microtuning keyn (midi-channel obj) stream str
                                         ;; pitch bend one sample before the note
                                         (1- time))
          (declare (type (signed-byte 8) keyn chan))
          (unless (< keyn 0)
            (midi-note stream time keyn
                       (the (or fixnum single-float) (midi-duration obj))
                       ampl chan))))
      (values))))

(defmethod write-event
    ((obj midi-event) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (jackmidi-output-stream))
    (typecase obj
      (midi-channel-event
       (incudine.edf:at (+ (incudine:now)
                           (* incudine.util:*sample-rate* scoretime))
         (midi-out stream
           (logior (ash (midi-event-opcode obj) 4) (midi-event-channel obj))
           (midi-event-data1 obj) (midi-event-data2 obj) 3))))))
;; (midi-write-message (midi-event->midi-message obj) str scoretime nil)

(defmethod write-event
           ((obj integer) (str incudine-stream) scoretime)
  (declare (ignore obj str scoretime))
  ;; (midi-write-message obj str scoretime nil)
  )

(defmethod write-event ((obj osc) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (osc-output-stream))
;;    (format t "~a~%" scoretime)
    (incudine:at (+ (incudine:now) (* incudine::*sample-rate* scoretime))
                 (lambda () (apply #'send-osc stream (osc-path obj)
                              (osc-types obj)
                              (let ((msg (osc-msg obj)))
                                (if (consp msg) msg (list msg)))))))
  (values))

#|

(defun output (msg &key at (to *out*) raw)
  (declare (ignore msg at to raw))
  (break "incudine-out")
  ;; (cond
  ;;  ((numberp msg)
  ;;   (incudine:writeshort (second (io-open to))
  ;;                        (or at (incudine:time))
  ;;                        (if raw msg
  ;;                            (midi-message->pm-message msg))))
  ;;  ((stringp msg)
  ;;   (incudine:writesysex (second (io-open to))
  ;;                        (or at (incudine:time)) msg))
  ;;  (t (incudine:write (second (io-open to)) msg raw)))
  (values))

(defun parse-pm-output (forms clauses ops)
  clauses
  ops
  (let ((head forms)
        (oper (pop forms))
        (expr nil)
        (args (list))
        (to nil)
        (loop '()))
    (when (null forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (setf expr (pop forms))
    (do ((stop nil))
        ((or stop (null forms)))
      (case (car forms)
        ((to :to)
         (when (null (cdr forms))
           (loop-error ops head "Missing '" oper " to' expression."))
         (setf args (nconc args (list ':to (cadr forms))))
         (setf to t)
         (setf forms (cddr forms)))
        ((at :at)
         (when (null (cdr forms))
           (loop-error ops head "Missing '" oper " at' expression."))
         (setf args (nconc args (list ':at (cadr forms))))
         (setf forms (cddr forms)))
        ((raw :raw)
         (when (null (cdr forms))
           (loop-error ops head "Missing '" oper
            " raw' expression."))
         (setf args (nconc args (list ':raw (cadr forms))))
         (setf forms (cddr forms)))
        (t (setf stop t))))
    (unless to (setf args (nconc args (list ':to '*out*))))
    (setf loop (list `(,oper ,expr ,@args)))
    (values (make-loop-clause 'operator oper 'looping loop) forms)))

(defparameter *process-operators*
  (append *process-operators*
          (list
           (list 'incudine:output #'parse-pm-output 'task 'to 'at
                 'raw))))
|#
