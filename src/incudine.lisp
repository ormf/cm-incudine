;;; incudine.lisp
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

(defun incudine-rts-hush ()
  (incudine:flush-pending)
  (dotimes (chan 16) (cm::sprout
                      (cm::new cm::midi-control-change :time 0
                        :controller 123 :value 127 :channel chan)))
;;;  (incudine::node-free-unprotected)
  (scratch::node-free-all)
  )

(setf (fdefinition 'rts-hush) #'incudine-rts-hush)

(defun set-standard-hush ()
  (setf (fdefinition 'rts-hush) #'incudine-rts-hush))

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
    ((obj midi-event) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (or (incudine-output str) (jackmidi-output-stream)))
;;    (break "write-event: ~a" obj)
    (typecase obj
         (midi-channel-event
          (at (+ (rts-now) scoretime)
              (progn
                (midi-out
                 stream
                 (logior (ash (midi-event-opcode obj) 4) (midi-event-channel obj))
                 (midi-event-data1 obj) (midi-event-data2 obj) 3)))))))
;; (midi-write-message (midi-event->midi-message obj) str scoretime nil)

(defmethod write-event ((obj integer) (str incudine-stream) scoretime)
  (alexandria:if-let (stream (or (incudine-output str) (jackmidi-output-stream)))
    (at (+ (rts-now) scoretime)
                     (midi-out
                      stream
                      (logior
                       (ash (channel-message-opcode obj) 4)
                       (channel-message-channel obj))
                      (channel-message-data1 obj)
                      (channel-message-data2 obj)
                      3))))


(defmethod write-event ((obj function) (str incudine-stream) scoretime)
  (declare (ignore str))
;;  (break "write-event (fn): ~a" obj)
  (at (+ (rts-now) scoretime) obj)
  (values))

