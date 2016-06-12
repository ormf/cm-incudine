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
           (when (not (io-open obj)))
           obj)

(defmethod close-io ((obj incudine-stream) &rest mode)
  (declare (ignore obj mode))
           (values))

(defmethod initialize-io ((obj incudine-stream))
  (declare (ignore obj))
  )

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

(defmethod write-event ((obj midi) (str incudine-stream) scoretime)
  (declare (ignore obj str scoretime))
  ;; (let ((keyn (midi-keynum obj))
           ;;       (chan (midi-channel obj))
           ;;       (ampl (midi-amplitude obj))
           ;;       (sched (scheduling-mode)))
           ;;   (ensure-velocity ampl keyn)
           ;;   (ensure-microtuning keyn chan str)
           ;;   (unless (< keyn 0)
           ;;     (incudine:writeshort (second (io-open str))
           ;;                          (if (eq sched ':events)
           ;;                              (+ (round (* scoretime 1000))
           ;;                                 (incudine-offset str))
           ;;                              (incudine:time))
           ;;                          (incudine:message
           ;;                           (logior 144 (logand chan 15))
           ;;                           (logand keyn 127)
           ;;                           (logand ampl 127)))
           ;;     (enqueue *qentry-message*
           ;;      (make-note-off chan keyn 127)
           ;;      (+ scoretime (midi-duration obj)) nil sched)
           ;;     )
           ;;   (values))
           )

(defmethod write-event
    ((obj midi-event) (str incudine-stream) scoretime)
  (break "write-event: ~a to ~a, time: ~a~%" obj str scoretime)
  (format t "write-event: ~a to ~a, time: ~a~%" obj str scoretime)
  ;; (midi-write-message (midi-event->midi-message obj) str
           ;;  scoretime nil)
           )

(defmethod write-event
           ((obj integer) (str incudine-stream) scoretime)
  (declare (ignore obj str scoretime))
  ;; (midi-write-message obj str scoretime nil)
  )

(defun output (msg &key at (to *out*) raw)
  (declare (ignore msg at to raw))
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

#|
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
