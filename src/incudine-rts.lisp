(in-package :cm)

(defparameter *incudine-default-input* nil)
(defparameter *incudine-default-output* nil)
(defparameter *incudine-default-latency* 5)
(defparameter *incudine-default-inbuf-size* 512)
(defparameter *incudine-default-outbuf-size* 2048)
(defparameter *incudine-default-filter* 0)
(defparameter *incudine-default-mask* 0)

(defvar *midi-in* nil)
(defvar *midi-out* nil)
(defvar *osc-in* nil)
(defvar *osc-out* nil)
(defvar *fudi-in* nil)
(defvar *fudi-out* nil)
(defparameter *rtsdebug* nil)

(declaim (special *osc-out*))

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

(defun midi-close-default (&rest args)
  (if (or (member :inout args)
          (member :input args)
          (not args))
      (if *midi-in*
          (progn
            (jackmidi:close *midi-in*)
            (setf *midi-in* nil))))
  (if (or (member :inout args)
          (member :output args)
          (not args))
      (if *midi-out*
          (progn
            (jackmidi:close *midi-out*)
            (setf *midi-out* nil)))))


(defun midi-open-default (&key (direction :input))
  (case direction
    (:output
     (progn
       (midi-close-default :output)
       (setf *midi-out* (jackmidi:open :direction :output))))
    (t (progn
         (midi-close-default :input)
         (setf *midi-in* (jackmidi:open :direction :input))))))

(defun samps->time (samps)
  (case *time-format*
    ((:sec) (/ samps incudine::*sample-rate*))
    ((:sample) samps)
    ((:ms) (/ samps incudine::*sample-rate* 0.001))))

(defun time->samps (time)
  (case *time-format*
    ((:sec) (* time incudine::*sample-rate*))
    ((:sample) time)
    ((:ms) (* time incudine::*sample-rate* 0.001))))

(defun time->ms (time)
  (case *time-format*
    ((:sec) (/ time 0.001))
    ((:sample) (/ time incudine::*sample-rate* 0.001))
    ((:ms) time)))

(defun secs->samps (secs)
  (* secs incudine::*sample-rate*))

(defun samps->secs (samps)
  (/ samps incudine::*sample-rate*))

(defun at (time function &rest args)
  (apply #'incudine:at (time->samps time) function args))

(defun amp->velo (amp)
  (round (* 127 (min 1 (max 0 amp)))))

(defun jackmidi-input-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*input-streams* 0)))

(defun jackmidi-output-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*output-streams* 0)))

(defun osc-output-stream ()
  *osc-out*)

(defun fudi-output-stream ()
  *fudi-out*)

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
  (at time (note-on stream pitch velo chan))
  (at (+ time (* incudine::*sample-rate* dur)) (note-off stream pitch 0 chan)))

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
                      (at time
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
      (let ((time (+ (now) scoretime)))
        (multiple-value-bind (keyn chan)
            (incudine-ensure-microtuning keyn (midi-channel obj) stream str
                                         ;; pitch bend one sample before the note
                                         (- time 1.0d0))
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
       (at (+ (rts-now) scoretime)
         (midi-out stream
           (logior (ash (midi-event-opcode obj) 4) (midi-event-channel obj))
           (midi-event-data1 obj) (midi-event-data2 obj) 3))))))
;; (midi-write-message (midi-event->midi-message obj) str scoretime nil)

(defmethod write-event ((obj integer) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (jackmidi-output-stream))
    (at (+ (rts-now) scoretime)
                     (midi-out
                      stream
                      (logior
                       (ash (channel-message-opcode obj) 4)
                       (channel-message-channel obj))
                      (channel-message-data1 obj)
                      (channel-message-data2 obj)
                      3))))

(defmethod write-event ((obj osc) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (osc-output-stream))
;;    (format t "scoretime: ~a~%" scoretime)
    (at (+ (rts-now) scoretime)
                 (lambda () (apply #'osc::send-osc stream (osc-path obj)
                              (osc-types obj)
                              (let ((msg (osc-msg obj)))
                                (if (consp msg) msg (list msg)))))))
  (values))


(defmethod write-event ((obj fudi) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (fudi-stream obj))
    (at (+ (rts-now) scoretime)
                 (lambda () (funcall #'send-fudi
                                (let ((msg (fudi-msg obj)))
                                  (if (consp msg) msg (list msg)))
                                :stream stream))))
  (values))

;;; dummy method to make set-receiver! happy
(defmethod rt-stream-receive-type ((stream jackmidi:input-stream))
  nil)

;;; dummy method to make set-receiver! happy
(define-setf-expander rt-stream-receive-type (stream)
  (declare (ignore stream))
  nil)

;;; dummy method to make set-receiver! happy
(defmethod object-name ((stream jackmidi:input-stream))
  stream)

;;; we need to store the responder for an input stream type globally for
;;; stream-receive-deinit
(defvar *stream-recv-responders* (make-hash-table))

(defmethod rt-stream-receive-data ((stream jackmidi:input-stream))
  (declare (ignore stream))
  nil)

(alexandria:define-constant +ml-opcode-mask+ #b11110000)
(alexandria:define-constant +ml-channel-mask+ #b1111)

(defmethod stream-receive-init ((stream jackmidi:input-stream) hook args)
  (if (gethash stream *stream-recv-responders*)
      (progn
        (incudine:remove-responder (gethash stream *stream-recv-responders*))
        (remhash stream *stream-recv-responders*)))
  (let* ((mask (getf args :mask #xffffffff))
         (responder (incudine:make-responder
                     stream
                     (lambda (st d1 d2)
                       (if (/= 0 (logand (ash 1 (+ 16 (ash st -4))) mask)) ;;; emulate portmidi input filtering
                           (let* ((opcode (ash (logand st +ml-opcode-mask+) -4))
                                  (channel (logand st +ml-channel-mask+))
                                  (mm (make-channel-message opcode channel d1 d2))
                                  (ms (time->ms (now))))
                             (funcall hook mm ms)))))))
    (if responder
        (setf (gethash stream *stream-recv-responders*) responder)
        (error "~a: Couldn't add responder!" stream)))
  (values))

(defmethod stream-receive-start ((stream jackmidi:input-stream) args)
  args
  (if (incudine::receiver-status
           (incudine::receiver *midi-in*))
      T
      (incudine:recv-start stream)))

(defmethod stream-receive-stop ((stream jackmidi:input-stream))
    (incudine:recv-stop stream)
  (values))

(defmethod stream-receive-deinit ((stream jackmidi:input-stream))
  (if (incudine:remove-responder (gethash stream *stream-recv-responders*))
      (error "~a: Couldn't remove responder!" stream)
      (remhash stream *stream-recv-responders*)))


(defun set-receiver! (hook stream &rest args)
  (let ((data (rt-stream-receive-data stream)))
    (if (and (not (null data)) (first data))
        (error "set-receiver!: ~s already receiving." stream)
        (let ((type (getf args ':receive-type)))
          (if type (setf (rt-stream-receive-type stream) type)
              (if (not (rt-stream-receive-type stream))
                  (setf (rt-stream-receive-type stream)
                          *receive-type*)))
          (stream-receive-init stream hook args)
          (cond
           ((stream-receive-start stream args)
            (format t "~%; ~a receiving!" (object-name stream)))
           (t (stream-receive-deinit stream)
            (error
             "set-receiver!: ~s does not support :receive-type ~s."
             stream (rt-stream-receive-type stream))))
          (values)))))

(defun rts-enqueue (handle object time start sched)
  ;; time is either in sec msec or usec
  ;; sched is either :rts or nil, nil means from repl
  ;; add check for sprout without rts running.
  (let ((repl? (not (eql sched ':rts)))
	(data 0)
	(flag 0))
    (cond ((= (logand handle #xF) *qentry-message*)
	   ;; integer midi messages can be inserted directly into the
	   ;; scheduler as data. could do C pointers this way too.
	   (setq data object))
	  ((= 0 (logandc2 handle #xF)) ; handle is less than 10000 (unsigned)
           ;; new entry, add to table
	   ;; if its a seq or a process we also have to cache the
	   ;; start time of the object: (<object> . start)
	   ;; start time is in *time-format* units 
           (cond ((= handle *qentry-process*)
;;;                    (format t "~2&rts-enqueue: time: ~10,0f, start: ~10,0f~%" time start)
                  (at time
                      (lambda ()
                        (let ((*rts-thread* t))
                          (scheduler-do-process object
                                                time
                                                start
                                                *rts-out*
                                                handle
                                                ':rts)))))
                 ((= handle *qentry-seq*)
                  (at time
                      (lambda ()
                        (let ((*rts-thread* t))
                          (scheduler-do-seq object
                                            time
                                            start
                                            *rts-out*
                                            handle
                                            ':rts)))))
                 ((= handle *qentry-object*)
                  (at time
                      (lambda () (output object :to *rts-out*)))))))

    (unless (eql flag 0)
      (case flag
        ((1) (error "enqueue: no room in scheduler for ~S." object))
        ((2) (error "enqueue: RTS not running."))))
    (values)))


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

#|
(defmacro rtsdebug (&rest args)
  `(if *rtsdebug* (format ,@args)))


(defun rts-enqueue (handle object time start sched)
  ;; time is either in sec msec or usec
  ;; sched is either :rts or nil, nil means from repl
  ;; add check for sprout without rts running.
  (let ((repl? (not (eql sched ':rts)))
	(data 0)
	(flag 0))
    (cond ((= (logand handle #xF) *qentry-message*)
	   ;; integer midi messages can be inserted directly into the
	   ;; scheduler as data. could do C pointers this way too.
	   (setq data object))
	  ((= 0 (logandc2 handle #xF))  ; handle is less than 10000 (unsigned)
           ;; new entry, add to table
	   ;; if its a seq or a process we also have to cache the
	   ;; start time of the object: (<object> . start)
	   ;; start time is in *time-format* units 
	   (when (or (= handle *qentry-seq*)
		     (= handle *qentry-process*))
	     (setq object (cons object start)))
	   ;; handle only has type information, make full handle
	   (setq handle (%newhandle handle))
	   ;; lock scheduler out during table set if in repl
	   (if repl? (rts:scheduler-lock-lisp))
	   ;; add new entry to table
	   (setf (gethash handle *qentries*) object)
	   ;; unlock table
	   (if repl? (rts:scheduler-unlock-lisp))
	   )
	  )
    (rtsdebug t "~%enqueing: data=~d type=~d time=~d repl=~d"
	      data handle time (if repl? 1 0))
    ;; convert to msec
    (setq flag (rts:scheduler-enqueue data ;;; = 0 if no *qentry-message*
				      handle ;;; hash-key + type in lower nibble
				      ;; convert seconds to usec for C
				      (if (eq rts:*time-format* ':sec)
					  (floor (* time 1000000))
					  time)
				      (if repl? 1 0)))
    (unless (eql flag 0)
      (case flag
        ((1) (error "enqueue: no room in scheduler for ~S." object))
        ((2) (error "enqueue: RTS not running."))))
    (values)))

(defun rtserr (fn args)
  (error "Attempt to call ~s without RTS loaded." (cons fn args)))

(defun cm-hook (data handle time)
  ;; C SIDE HAS LOCKED LISP DURING EXTENT OF CALLBACK
  (let ((entry nil)
        (etype (%handle-type handle))) ; get entry type from low nibble
    ;; C time is usec or msec, convert to SEC if necessary
    (when (eq? rts:*time-format* ':sec)
      (setq time (/ time 1000000.0)))
    (setq entry (if (= etype *qentry-message*)
		    data
		    (or (gethash handle *qentries*)
			(error "No RTS entry for handle ~D."
			       handle))))   
    (cond ((= etype *qentry-process*)
           ;; entry is (<process> . <start>)
	   (rtsdebug t "~&process=~s, start=~s time=~s~%"
		     (car entry) (cdr entry) time)
           (destructuring-bind (process . start) entry
             (scheduler-do-process process
                                   time
                                   start
                                   *rts-out*
                                   handle
                                   ':rts)))
          ((= etype *qentry-seq*)
	   ;; entry is ( (<seq> . <subobjects>) . <start>)
	   (rtsdebug t "~&seq=~s, start=~d time=~d~%" 
		     (caar entry) (cdr entry) time)
           (destructuring-bind (seq . start) entry
             (scheduler-do-seq seq
                               time
                               start
                               *rts-out*
                               handle
                               ':rts)))
          (t
	   (rtsdebug t "~&object=~s time=~s~%" entry time)
           (write-event entry *rts-out* time)))
    (values)))

;;;
;;; user level functions
;;; 



(defun rts (&rest args)
  (unless (rts:scheduler-state? ':stopped)
    (error "rts: scheduler is already running or paused."))
  (unless (or (null? args) (keywordp (car args)))
    (setq *rts-out* (pop args)))
  (unless (null? *rts-out*)  ;; initalize must be called for microtuning
    (initialize-io *rts-out*))
  (apply #'rts:scheduler-start args)
  (format t "~&; RTS running~%")
  (values))

(defun rts? (&optional arg)
  (apply #'rts:scheduler-state? arg))

(defun rts-pause () 
  (rts:scheduler-pause)
  (values))

(defun rts-continue ()
  (rts:scheduler-continue)
  (values))
(defun rts-flush ()
  (rts:scheduler-flush)
  (values))

(defun rts-hush ()
  (rts-flush)
  (when *rts-out*
    (do ((i 0 (+ i 1)))
	((>= i 16))
      (write-event (make <midi-control-change> :time 0
			 :controller 64 :value 0 :channel i)
		   *rts-out* 0)
      (write-event (make <midi-control-change> :time 0
			 :controller 123 :value 0)
		   *rts-out* 0)))
  (values))

(defun rts-reset () 
  ;;(print :rts-reset)
  #+openmcl (ccl:gc)
  #+sbcl (sb-ext:gc)
  (setq *qcount* 0)
  (clrhash *qentries*)
  (rts-reset-globals)
  (values))

(defun rts-reset-globals ()
  ;;(print :rts-reset-globals)
  (setq *rts-pstart* nil)
  (setq *rts-qtime* nil)
  (values))

(defun rts-stop ()
  (rts:scheduler-stop)
  (format t "~&; RTS stopped.~%")
  (values))

(defun rts-thread? () (rts:rts-thread? ))

(eval-when (:load-toplevel :execute)
  (rts:scheduler-hook-set! #'cm-hook)
  (rts:scheduler-add-hook! ':before-start #'rts-reset)
  (rts:scheduler-add-hook! ':after-stop #'rts-reset)
  (rts:scheduler-add-hook! ':error-continue #'rts-reset-globals)
  )
|#


#|


(defvar *rts-running* nil)

(defun rts (&rest args) args (setf *rts-running* t))

(defun rts? (&rest args) args *rts-running*)

(defun rts-stop (&rest args) args (setf *rts-running* nil))

(defun rts-pause (&rest args) (rtserr 'rts-pause args))

(defun rts-continue (&rest args) (rtserr 'rts-continue args))

(defun rts-enqueue (&rest args) args)

(defun rts-now (&rest args) (rtserr 'rts-now args))

(defun rts-thread? () nil)
|#
