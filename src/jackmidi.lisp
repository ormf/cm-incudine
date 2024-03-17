;;; jackmidi.lisp
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

(defconstant +ml-opcode-mask+ #xf0)
(defconstant +ml-channel-mask+ #x0f)
(defparameter *midi-in1* nil)
(defparameter *midi-out1* nil)
(defparameter *stream-recv-responders* (make-hash-table))
(defparameter  *midi-rcv-type-dummy* nil)
(defparameter  *midi-obj-name-dummy* nil)
(defparameter *rt-scale* 1) ;;; time scaling for realtime output

(defparameter *ml-opcodes*
  `((,+ml-control-change-opcode+ . :cc)
    (,+ml-note-on-opcode+ . :note-on)
    (,+ml-note-off-opcode+ . :note-off)
    (,+ml-program-change-opcode+ . :pgm-change)
    (,+ml-pitch-bend-opcode+ . :pitch-bend)
    (,+ml-key-pressure-opcode+ . :key-pressure)
    (,+ml-channel-pressure-opcode+ . :channel-pressure)))


(defun status->opcode (st)
  (cdr (assoc (ash (logand st +ml-opcode-mask+) -4)
              *ml-opcodes*)))

(defun status->channel (st)
  (logand st +ml-channel-mask+))

(defun make-mm-mask (opcode channels)
  "make a mask using midi opcode and channel mask for midi input
filtering."
  (+ (ash opcode 4) channels))

;;; add jackmidi stream structs to the special targets of the 'events
;;; function:

(push (list #'typep 'jackmidi:stream) *special-event-streams*)

(defmethod open-io ((obj jackmidi:stream) dir &rest args)
  (declare (ignore dir args))
;;;  (format t "open-io: ~a, io-open: ~a" obj (io-open obj))
           (when (not (io-open obj)))
           obj)

(defmethod close-io ((obj jackmidi:stream) &rest mode)
  (declare (ignore obj mode))
  (values))

(defun midi-close-default (&rest args)
  (if (or (member :inout args)
          (member :input args)
          (not args))
      (if *midi-in1*
          (progn
            (jackmidi:close *midi-in1*)
            (setf *midi-in1* nil))
          (jackmidi:close "midi_in-1")))
  (if (or (member :inout args)
          (member :output args)
          (not args))
      (if *midi-out1*
          (progn
            (jackmidi:close *midi-out1*)
            (setf *midi-out1* nil))
          (jackmidi:close "midi_out-1"))))

(defun midi-open-default (&key (direction :input) portname)
  (case direction
    (:output
     (progn
       (midi-close-default :output)
       (setf *midi-out1*
             (or (first (jackmidi:all-streams :output))
                 (jackmidi:open :direction :output :port-name (if portname portname "midi_out-1"))))
       (if *rts-out* (setf (incudine-output *rts-out*) *midi-out1*))))
    (t (progn
         (midi-close-default :input)
         (setf *midi-in1*
               (or (first (jackmidi:all-streams :input))
                   (jackmidi:open :direction :input
                                  :port-name (if portname portname "midi_in-1"))))
         (if *rts-out* (setf (incudine-input *rts-out*) *midi-in1*))))))

(defun jackmidi-input-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*input-streams* 0)))

(defun jackmidi-output-stream ()
  (unless (zerop (length jackmidi::*output-streams*))
    (elt jackmidi::*output-streams* 0)))

(declaim (inline midi-out))
(defun midi-out (stream status data1 data2 data-size)
  "create a closure to defer a call to jm_write_event."
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

(defun tempo-change (stream tempo)
  "wrapper for midi tempo-change messages."
  (declare (ignore stream))
  (setf *rt-scale* (/ 60 tempo)))

(declaim (inline midi-note))
(defun midi-note (stream time pitch dur velo chan)
  (at time (note-on stream pitch velo chan))
  (at (+ time dur) (note-off stream pitch 0 chan)))

(defmethod write-event ((obj midi) (str incudine-stream) scoretime)
  (declare (type (or fixnum float) scoretime))
  (alexandria:if-let (stream (incudine-output str))
;;    (format t "~a~%" scoretime)
;;;    (break "write-event (incudine-stream):")
    (multiple-value-bind (keyn ampl)
        (incudine-ensure-velocity (midi-keynum obj) (midi-amplitude obj))
      (declare (type (integer 0 127) ampl))
      (let ((time (+ (rts-now) (* *rt-scale* scoretime))))
        (let ((keyn (keynum keyn)))
          (unless (< keyn 0)
            (multiple-value-bind (keyn chan)
                (incudine-ensure-microtuning (coerce keyn 'single-float)
                                             (midi-channel obj) str
                                             ;; pitch bend before the note
                                             (- time 1e-5))
              (declare (type (signed-byte 8) keyn chan))
              (unless (< keyn 0)
                (midi-note stream time keyn
                           (float (midi-duration obj))
                           ampl chan))))))
      (values))))

(defmethod write-event ((obj midi) (str jackmidi:output-stream) scoretime)
  (declare (type (or fixnum float) scoretime))
  (alexandria:if-let (stream (incudine-output str))
;;    (format t "~a~%" scoretime)
;;    (break "write-event (midi): ~a~%~a~%" obj str)
    (multiple-value-bind (keyn ampl)
        (incudine-ensure-velocity (floor (midi-keynum obj)) (midi-amplitude obj))
      (declare (type (integer 0 127) ampl))
      (let ((time (+ (rts-now) (* *rt-scale* scoretime))))
        (with-slots (duration channel) obj
          (declare (type (signed-byte 8) keyn channel))
          (unless (< keyn 0)
            (midi-note stream time keyn
                       (float duration)
                       ampl channel))))
      (values))))

;;; dummy method to make set-receiver! happy
(defmethod rt-stream-receive-type ((stream jackmidi:input-stream))
  *midi-rcv-type-dummy*)

;;; dummy method to make set-receiver! happy
(defmethod object-name ((stream jackmidi:input-stream))
  *midi-obj-name-dummy*)

(defmethod rt-stream-receive-data ((stream jackmidi:input-stream))
  (declare (ignore stream))
  nil)

#|
(defmethod stream-receive-init ((stream jackmidi:input-stream) hook args)
  (if (gethash stream *stream-recv-responders*)
      (progn
        (incudine:remove-responder (gethash stream *stream-recv-responders*))
        (remhash stream *stream-recv-responders*)))
  (let* ((mask (getf args :mask #xffff))
         (responder (incudine:make-responder
                     stream
                     (lambda (st d1 d2)
                       (if (/= 0 (logand st mask)) ;;; input filtering
                           (let* ((opcode (ash (logand st +ml-opcode-mask+) -4))
                                  (channel (logand st +ml-channel-mask+))
                                  (mm (make-channel-message opcode channel d1 d2))
                                  (ms (time->ms (now))))
                             (funcall hook st d1 d2)))))))
    (if responder
        (setf (gethash stream *stream-recv-responders*) responder)
        (error "~a: Couldn't add responder!" stream)))
  (values))
|#

(defmethod stream-receive-init ((stream jackmidi:input-stream) hook args)
  (if (gethash stream *stream-recv-responders*)
      (progn
        (incudine:remove-responder (gethash stream *stream-recv-responders*))
        (remhash stream *stream-recv-responders*)))
  (let* ((mask (getf args :mask #xffff))
         (format (getf args :format :mm))
         (responder (case format
                      (:raw
                       (incudine:make-responder
                        stream
                        (lambda (st d1 d2)
                          (funcall hook st d1 d2))))
                      (:mm
                       (incudine:make-responder
                        stream
                        (lambda (st d1 d2)
                          (let* ((opcode (ash (logand st +ml-opcode-mask+) -4))
                                 (channel (logand st +ml-channel-mask+))
                                 (mm (make-channel-message opcode channel d1 d2))
                                 (ms (time->ms (now))))
                            (funcall hook mm ms))))))))
    (declare (ignorable mask))
    (if responder
        (setf (gethash stream *stream-recv-responders*) responder)
        (error "~a: Couldn't add responder!" stream)))
  (values))

(defmethod stream-receive-start ((stream jackmidi:input-stream) args)
  args
  (or (and (incudine::receiver stream)
           (incudine::receiver-status
            (incudine::receiver stream)))
      (incudine:recv-start stream)))

(defmethod stream-receive-stop ((stream jackmidi:input-stream))
    (incudine:recv-stop stream)
  (values))

(defmethod stream-receive-deinit ((stream jackmidi:input-stream))
  (if (incudine:remove-responder (gethash stream *stream-recv-responders*))
      (error "~a: Couldn't remove responder!" stream)
      (remhash stream *stream-recv-responders*)))

(defun incudine-ensure-microtuning (keyn chan stream time)
  "return values keynum and chan according to tuning specs in stream."
  (declare (type (or fixnum single-float double-float symbol) keyn)
           (type (integer 0 15) chan)
           (type (or jackmidi:output-stream cm:incudine-stream) stream)
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
             (setf dat (midi-stream-tunedata stream))
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

(export '(jackmidi-input-stream jackmidi-output-stream
           midi-out ctl-out note-on note-off pitch-bend pgm-change midi-note midi-write-message
          midi-open-default midi-close-default
          incudine-ensure-microtuning *rt-scale* *midi-in1* *midi-out1* *midi-rcv-type-dummy*
          *midi-obj-name-dummy*)
        'cm)


#|

;;; todo: Adapt the following definitions to incudine's
;;; streaming protocol to get full integration into cm's midi
;;; capabilities (like channel-tuning, process integration
;;; with special clauses, etc.).
;;;
;;; This is distinct from incudine-rts which aims to be able to
;;; mix midi, fudi and functions in the same rt-process.
;;;
;;;
;;; It needs checking whether this could be integrated into
;;; incudine-rts as well. Otherwise jackmidi-stream could serve as
;;; a standalone protocol for midi only rt-streams.
;;;
;;; Use pm.lisp as reference.

(defparameter *jackmidi-default-input* nil)
(defparameter *jackmidi-default-output* nil)
(defparameter *jackmidi-default-latency* 5)
(defparameter *jackmidi-default-inbuf-size* 512)
(defparameter *jackmidi-default-outbuf-size* 2048)
(defparameter *jackmidi-default-filter* 0)
(defparameter *jackmidi-default-mask* 0)

(progn
 (defclass jackmidi-stream (rt-stream midi-stream-mixin)
           ((input :initform *jackmidi-default-input* :initarg :input
             :accessor jackmidi-input)
            (output :initform *jackmidi-default-output* :initarg
             :output :accessor jackmidi-output)
            (latency :initform *jackmidi-default-latency* :initarg
             :latency :accessor rt-stream-latency)
            (inbufsize :initform *jackmidi-default-inbuf-size*
             :initarg :inbuf-size :accessor jackmidi-inbuf-size)
            (outbufsize :initform *jackmidi-default-outbuf-size*
             :initarg :outbuf-size :accessor jackmidi-outbuf-size)
            (receive-data :initform (list nil nil nil nil) :accessor
             rt-stream-receive-data)
            (receive-mode :initform :message :initarg :receive-mode
             :accessor rt-stream-receive-mode)
            (filter :initform *jackmidi-default-filter* :initarg
             :filter :accessor jackmidi-filter)
            (mask :initform *jackmidi-default-mask* :initarg
             :channel-mask :accessor jackmidi-channel-mask)
            (offset :initform 0 :initarg :offset :accessor
             jackmidi-offset))
           #+metaclasses  (:metaclass io-class))
 (defparameter <jackmidi-stream> (find-class 'jackmidi-stream))
 (finalize-class <jackmidi-stream>)
;;; (setf (io-class-file-types <jackmidi-stream>) '("*.ic"))
 (values))


(defmethod open-io ((obj jackmidi-stream) dir &rest args)
  (declare (ignore dir args))
;;;  (format t "open-io: ~a, io-open: ~a" obj (io-open obj))
           (when (not (io-open obj)))
           obj)

(defmethod close-io ((obj jackmidi-stream) &rest mode)
  (declare (ignore obj mode))
  (values))

|#
