;;; incudine-rts.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cm)

(defparameter *incudine-default-input* nil)
(defparameter *incudine-default-output* nil)
(defparameter *incudine-default-latency* 5)
(defparameter *incudine-default-inbuf-size* 512)
(defparameter *incudine-default-outbuf-size* 2048)
(defparameter *incudine-default-filter* 0)
(defparameter *incudine-default-mask* 0)
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

(defmethod incudine-output ((obj #+portaudio pm:output-stream #-portaudio jackmidi:output-stream))
               obj)

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

(defun samps->time (samps)
  (case *time-format*
    ((:sec) (* samps incudine.util:*sample-duration*))
    ((:sample) samps)
    ((:ms) (* samps incudine.util:*sample-duration* 1000))))

(defun time->samps (time)
  (case *time-format*
    ((:sec) (* time incudine.util:*sample-rate*))
    ((:sample) time)
    ((:ms) (* time incudine.util:*sample-rate* 0.001))))

(defun time->ms (time)
  (case *time-format*
    ((:sec) (* time 1000.0))
    ((:sample) (* time incudine.util:*sample-duration* 1000))
    ((:ms) time)))

(defun secs->samps (secs)
  (* secs incudine.util:*sample-rate*))

(defun samps->secs (samps)
  (* samps incudine.util:*sample-duration*))

(defun at (time function &rest args)
  (apply #'incudine:at (time->samps time) function args))

(defun amp->velo (amp)
  (round (* 127 (min 1 (max 0 amp)))))


(defun fudi-output-stream ()
  *fudi-out*)

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

(defmethod write-event
    ((obj midi-program-change) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (or (incudine-output str)))
    (let* ((dat (midi-stream-tunedata str)))
      (destructuring-bind (offs num)
          (cond
            ((or (null dat) (eq (first dat) t))
             (list (midi-event-channel obj) 1))
            (t dat))
        (dotimes (i num)
          (at (+ (rts-now) scoretime)
              (midi-out
               stream
               (logior (ash (midi-event-opcode obj) 4) (+ i offs))
               (midi-event-data1 obj) (midi-event-data1 obj) 3)))))))

(defmethod write-event
    ((obj midi-program-change) (str #+portaudio pm:output-stream #-portaudio jackmidi:output-stream) scoretime)
  (alexandria:if-let (stream (or (incudine-output str)))
    (let* ((dat (midi-stream-tunedata str)))
      (destructuring-bind (offs num)
          (cond
            ((or (null dat) (eq (first dat) t))
             (list (midi-event-channel obj) 1))
            (t dat))
        (dotimes (i num)
          (at (+ (rts-now) scoretime)
              (midi-out
               stream
               (logior (ash (midi-event-opcode obj) 4) (+ i offs))
               (midi-event-data1 obj) (midi-event-data1 obj) 3)))))))

(defmethod write-event
    ((obj midi-event) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (incudine-output str))
;;;    (break "write-event (midi-event): ~a" obj)
    (typecase obj
         (midi-channel-event
          (at (+ (rts-now) scoretime)
              (progn
                (midi-out
                 stream
                 (logior (ash (midi-event-opcode obj) 4) (midi-event-channel obj))
                 (round (midi-event-data1 obj)) (or (round (midi-event-data2 obj)) 0) 3)))))))

(defmethod write-event
    ((obj midi-event) (stream #+portaudio pm:output-stream #-portaudio jackmidi:output-stream) scoretime)
  (typecase obj
    (midi-channel-event
     (at (+ (rts-now) scoretime)
         (progn
           (midi-out
            stream
            (logior (ash (midi-event-opcode obj) 4) (midi-event-channel obj))
            (round (midi-event-data1 obj)) (or (round (midi-event-data2 obj)) 0) 3))))))
;; (midi-write-message (midi-event->midi-message obj) str scoretime nil)

(defmethod write-event ((obj integer) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (incudine-output str))
    (at (+ (rts-now) scoretime)
                     (midi-out
                      stream
                      (logior
                       (ash (channel-message-opcode obj) 4)
                       (channel-message-channel obj))
                      (channel-message-data1 obj)
                      (channel-message-data2 obj)
                      3))))

(defmethod write-event ((obj integer) (stream #+portaudio pm:output-stream #-portaudio jackmidi:output-stream) scoretime)
  (at (+ (rts-now) scoretime)
      (midi-out
       stream
       (logior
        (ash (channel-message-opcode obj) 4)
        (channel-message-channel obj))
       (channel-message-data1 obj)
       (channel-message-data2 obj)
       3)))


(defmethod write-event ((obj function) (str incudine-stream) scoretime)
  (declare (ignore str))
;;  (break "write-event (fn): ~a" obj)
  (at (+ (rts-now) scoretime) obj)
  (values))

(defmethod write-event ((obj function) (str #+portaudio pm:output-stream #-portaudio jackmidi:output-stream) scoretime)
  (declare (ignore str))
;;  (break "write-event (fn): ~a" obj)
  (at (+ (rts-now) scoretime) obj)
  (values))

#|

(defun rts ()
(make-instance 'incudine-stream)  (setf *rts-out* (new <incudine-stream>))
  (incudine:rt-start))

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

(export '(*rts-out* incudine-stream
           samps->time time->samps secs->samps samps->secs at amp->velo
           jackmidi≈-input-stream jackmidi-output-stream osc-output-stream fudi-output-stream
           midi-out ctl-out note-on note-off pitch-bend pgm-change midi-note midi-write-message
           incudine-ensure-microtuning write-event rts-enqueue
           midi-open-default midi-close-default

           *fudi.in* *fudi-out* fudi fudi-open-default fudi-open fudi-close-default
           send-fudi
))
|#

(defun ensure-jackmidi (stream)
  (if(typep stream 'incudine-stream)
     (incudine-output stream)
     stream))

(export '(rts *rts-out* incudine-stream incudine-output incudine-input
          samps->time time->samps secs->samps samps->secs at amp->velo
          ensure-jackmidi
          write-event)
        :cm)
