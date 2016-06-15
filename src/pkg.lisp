;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name$
;;; $Revision: 1531 $
;;; $Date: 2008-01-09 18:37:03 +0100 (Wed, 09 Jan 2008) $

(in-package :cl-user)

;;;
;;; defstub: define entry points of unloaded systems
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro defstub (name &optional args type)
  ;; stub out defuns, defmethods and global vars of systems that are
  ;; not currently loaded. If stub is called then signal consistent
  ;; error message:
  ;; "Attempt to call ($FN ...) without $SYS loaded."
  (cond ((consp name)
         (ecase (car name)
           (setf 
            (unless type (setq type :defun))
            (unless args (setq args '(a b))))
           (special
            (setq args nil)
            (setq type ':proclaim))))
        (t
         (unless type (setq type :defun))
         (unless args (setq args '(&rest args)))))
  (let* ((vars (loop for x in args
                  for n = (if (consp x) (car x) x)
                  unless (member n lambda-list-keywords)
                  collect n))
         (str (if (eql (car args) '&rest)
                  (format nil
                          "Attempt to call (~A:~A~~{ ~~S~~}) without ~A loaded."
                          (package-name *package*) 
			  name
			  (package-name *package*))
                  (format nil "Attempt to call (~A:~A~{~A~}) without ~A loaded."
                          (package-name *package*)
			  name
                          (loop repeat (length vars) collect " ~S")
                          (package-name *package*)))))
    vars str
    (ecase type 
      (:method `(progn (defgeneric ,name ,args)
                       (defmethod ,name ,args 
                         ;; try to stop cmu from optimizing out stub
                         ;; calls based on analysis of error condition
                         ;; negating any return values
                         #+cmu (if *package* (error ,str ,@vars) , (car vars))
                         #-cmu (error ,str ,@vars)
                         )))
      (:defun `(defun ,name ,args
                 #+cmu (if *package* (error ,str ,@vars) ,(car vars))
                 #-cmu (error ,str ,@vars)
                 ))      
      (:proclaim `(proclaim (quote ,name)))))))

(export '(defstub) :cl-user)

;;;
;;; CLM package stubs
;;;

#-clm
(defpackage :clm
  (:use :common-lisp)
  #+openmcl (:import-from :ccl #:open-shared-library)
  (:import-from :cl-user #:defstub)
  (:export #:mus-next #:mus-bshort #:mus-aifc #:mus-riff
           #:mus-lshort #:*clm-with-sound-depth* #:wsdat-play
           #:init-with-sound #:finish-with-sound #:*clm-file-name*          
           #:*clm-channels* #:*clm-srate*
           ;; these symbols are  used by cm but not redefined
           #:spectrum #:env #:src #:clm-load #:dac #:definstrument
           #:*definstrument-hook* #:with-sound #:filter #:delay))

(in-package :clm)

#-clm
(progn
(defstub clm-load)
(defstub dac)
(defstub init-with-sound)
(defstub finish-with-sound)
(defstub wsdat-play)
(defstub (setf wsdat-play))
(defmacro with-sound ((&rest args) &body body)
  args body
  (error "Attempt to call with-sound without CLM loaded."))
(defstub (special mus-next mus-aifc mus-bshort mus-riff
                           mus-lshort *clm-with-sound-depth*
                           *clm-file-name* *clm-channels*
                           *clm-srate* *definstrument-hook*))
)

;;;
;;; CMN package stubs
;;;

#-cmn
(defpackage :cmn
  (:use :common-lisp)
  (:import-from :cl-user #:defstub)
  (:export #:*exact-rhythms* #:staff-descriptors #:init-clm-input
           #:score #:stfdat-staff #:staff-data #:set-staff-number
           #:set-staff-clef #:finish-clm-input #:find-staff #:add-staff
           #:add-data-1 #:add-note-to-staff))

(in-package :cmn)

#-cmn
(progn
(defstub stfdat-staff )
(defstub staff-data (x) :method )
(defstub (setf staff-data) (a b) :method)
(defstub set-staff-number)
(defstub set-staff-clef)
(defstub finish-clm-input)
(defstub find-staff)
(defstub add-staff )
(defstub add-data-1)
(defstub init-clm-input)
(defstub add-note-to-staff)
(defstub (special *exact-rhythms* staff-descriptors))
)

;;;
;;; Fomus stubs
;;;

#-fomus
(defpackage :fomus
  (:use :common-lisp)
;  #+(or allegro lispworks clisp sbcl) (:shadow #:rest)
  (:import-from :cl-user #:defstub)
  (:export #:fomus #:event-base #:part #:note #:rest #:timesig
           #:keysig #:meas #:event-off #:obj-id #:obj-partid
           #:part-opts #:*parts* #:event-note #:event-dur
           #:part-events #:make-part #:make-note #:get-instr-syms
	   #:fomus-file))

(in-package :fomus)

#-fomus
(progn
;(defclass event-base ()
;  ((off :accessor event-off :initarg :off)
;   (partid :accessor event-partid :initarg :partid)))
;(defclass note (event-base) 
;  ((dur :accessor event-dur :initarg :dur)
;   (marks :accessor event-marks :initarg :marks)
;   (note :accessor event-note :initarg :note)))
;(defclass part ()
;  ((name :accessor part-name :initarg :name)
;   (events :accessor part-events :initarg :events)
;   (instr :accessor part-instr :initarg :instr)
;   (opts :accessor part-opts :initarg :opts)
;   (partid :accessor part-partid :initarg :partid)))

(defclass fomusobj-base ()
 ((id :accessor obj-id :initform nil :initarg :id)))
(defclass event-base (fomusobj-base)
 ((off :accessor event-off :initform nil :initarg :off)
  (partid :accessor event-partid :initform nil :initarg :partid)))
(defclass mark (event-base)
 ((off) (marks :accessor event-marks :initform nil :initarg :marks)
  (voice :accessor event-voice :initform 1 :initarg :voice)))
(defclass dur-base (mark)
 ((dur :accessor event-dur :initform 1 :initarg :dur)))
(defclass note (dur-base)
 ((note :accessor event-note :initform nil :initarg :note)))
(defclass part (fomusobj-base)
 ((name :accessor part-name :initform nil :initarg :name)
  (abbrev :accessor part-abbrev :initform nil :initarg :abbrev)
  (opts :accessor part-opts :initform nil :initarg :opts)
  (events :accessor part-events :initform nil :initarg :events)
  (instr :accessor part-instr :initform nil :initarg :instr)
  (props :accessor part-props :initform nil :initarg :props)
  (partid :accessor part-partid :initform nil :initarg :partid)))

(defstub obj-partid (x) :method)
(defstub fomus)
(defstub make-part)
(defstub make-note)
(defstub get-instr-syms)
(defstub fomus-file)
(defstub (special *parts*))
)

;;; rts

#-rts
(defpackage :rts
  (:use :common-lisp)
  (:import-from :cl-user #:defstub)
  (:export #:scheduler-start #:scheduler-stop
	   #:scheduler-pause #:scheduler-continue #:scheduler-flush
	   #:scheduler-state? #:scheduler-enqueue
	   #:scheduler-lock-lisp #:scheduler-unlock-lisp
	   #:scheduler-add-hook! #:scheduler-remove-hook!
	   #:scheduler-hook-set! #:scheduler-time #:*time-format*
	   #:*priority* #:*policy* #:*resolution* #:*error-format*
	   #:current-thread #:rts-thread?
	   #:random-seed #:between #:rescale #:odds #:pickl #:drunk 
	   #:wander #:shuffle #:interpl #:rhythm))

(in-package :rts)

#-rts
(progn
(defstub scheduler-start)
(defstub scheduler-stop)
(defstub scheduler-pause)
(defstub scheduler-continue)
(defstub scheduler-flush)
(defstub scheduler-state?)
(defstub scheduler-enqueue)
(defstub scheduler-lock-lisp)
(defstub scheduler-unlock-lisp)
(defstub scheduler-add-hook!)
(defstub scheduler-remove-hook!)
(defstub scheduler-hook-set!)
(defstub enqueue)
(defstub scheduler-time)
(defstub (special *time-format* *priority* *policy* 
		  *resolution* *error-format*))
(defstub current-thread)
(defstub rts-thread)
(defstub random-seed)
(defstub between)
(defstub rescale)
(defstub odds)
(defstub pickl)
(defstub drunk)
(defstub wander)
(defstub shuffle)
(defstub interpl)
(defstub rhythm)
)

#|
;;;
;;; Portmidi
;;;

#-portmidi
(defpackage :portmidi
  (:use :common-lisp) 
  (:nicknames :pm)
  (:shadow :initialize :terminate :time :start :stop :abort 
           :close :read :write :poll :now :output)
  #+openmcl (:import-from :ccl #:open-shared-library)
  (:import-from :cl-user #:defstub)
  (:export #:portmidi #:*portmidi* #:GetDefaultInputDeviceID
           #:GetDefaultOutputDeviceID #:GetDeviceInfo
           #:Start #:Time #:OpenInput #:OpenOutput
           #:Close #:SetFilter #:SetChannelMask #:Poll #:Read
           #:Message.status #:Message.data1 #:Message.data2 #:Message
           #:WriteShort #:Write #:WriteSysEx #:EventBufferFree
           #:EventBufferNew #:EventBufferMap #:EventBufferSet))

(in-package :portmidi)

#-portmidi
(progn
(defstub (special *portmidi*))
(defstub portmidi)
(defstub GetDefaultInputDeviceID)
(defstub GetDefaultOutputDeviceID)
(defstub GetDeviceInfo)
(defstub Start)
(defstub Time)
(defstub OpenInput)
(defstub OpenOutput)
(defstub Close)
(defstub SetFilter)
(defstub SetChannelMask)
(defstub Poll)
(defstub Read)
(defstub Message.status)
(defstub Message.data1)
(defstub Message.data2)
(defstub Message)
(defstub Write)
(defstub WriteShort)
(defstub WriteSysEx)
(defstub EventBufferFree)
(defstub EventBufferNew)
(defstub EventBufferSet)
(defstub EventBufferMap)
)

#-midishare
(defpackage :midishare
  (:use :common-lisp)
  (:nicknames :ms)
  (:import-from :cl-user #:defstub)
  (:export #:midishare #:midiGetVersion #:MidiOpen #:MidiClose
           #:MidiCountAppls #:MidiGetNamedAppl #:MidiGetIndAppl
           #:MidiErrIndex #:MidiGetName #:MidiConnect #:MidiGetTime
           #:MidiIsConnected #:MidiSendIm #:MidiSend #:MidiSendAt
           #:MidiCountEvs #:typeNote #:typeKeyOn #:typeKeyOff
           #:typeKeyPress #:typeCtrlChange #:typeProgChange
           #:typeChanPress #:typePitchWheel #:typePitchBend #:typeSongPos
           #:typeSongSel #:typeClock #:typeStart #:typeContinue #:typeStop
           #:typeTune #:typeActiveSens #:typeReset #:typeSysEx
           #:typeStream #:typePrivate #:typeSeqNum #:typeTextual
           #:typeCopyright #:typeSeqName #:typeInstrName #:typeLyric
           #:typeMarker #:typeCuePoint #:typeChanPrefix #:typeEndTrack
           #:typeTempo #:typeSMPTEOffset #:typePortPrefix #:typeKeySign
           #:typeTimeSign #:MidiNewEv #:port #:chan #:field #:bend #:text
           #:pitch #:vel #:dur #:ref #:date #:evtype #:MidiCopyEv
	   #:MidiFreeEv #:MidiAddField #:MidiTask #:MidiSetRcvAlarm
	   #:MidiGetEv #:nullptrp #:nullptr #:MidiFlushEvs
           ;; player
           #:OpenPlayer #:ClosePlayer #:midiNewSeq #:MidiAddSeq
           #:StartPlayer #:ContPlayer #:StopPlayer #:PausePlayer
           #:kMuteOn #:kMuteOff #:kSoloOn #:kSoloOff #:kMute
           #:kSolo #:kExternalSync #:kInternalSync #:kClockSync
           #:kSMPTESync #:GetAllTrackPlayer #:SetAllTrackPlayer
           #:GetTrackPlayer #:SetTrackPlayer #:SetParamPlayer
           #:SetTempoPlayer #:TicksPerQuarterNote
           #:SetSynchroInPlayer #:MidiNewMidiFileInfos
           #:MidiFileLoad #:MidiFileSave #:mf-clicks #:mf-format
           #:mf-timedef #:MidiFreeMidiFileInfos #:MidiFreeSeq
           #:midishare-receive #:midishare-receive-stop))

(in-package :midishare)

#-midishare
(progn
(defparameter %no-midishare 0) ;; stop CMU compiler optimization
(defun midishare ()
  ;; return false since MidiShare is not around
  %no-midishare)
(defstub midiGetVersion)
(defstub MidiOpen)
(defstub MidiClose)
(defstub MidiCountAppls)
(defstub MidiCountEvs)
(defstub MidiGetNamedAppl)
(defstub MidiGetIndAppl)
(defstub MidiErrIndex)
(defstub MidiGetName)
(defstub MidiConnect)
(defstub MidiGetTime)
(defstub MidiIsConnected)
(defstub MidiSendIm)
(defstub MidiSend)
(defstub MidiSendAt)
(defstub MidiNewEv)
(defstub port)
(defstub chan)
(defstub pitch)
(defstub vel)
(defstub dur)
(defstub field)
(defstub bend)
(defstub text)
(defstub ref)
(defstub date)
(defstub evtype)
(defstub MidiCopyEv)
(defstub MidiFreeEv)
(defstub MidiAddField)
(defstub nullptrp)
(defstub nullptr)
(defstub MidiFlushEvs)
(defstub MidiTask)
(defstub MidiSetRcvAlarm)
(defstub MidiGetEv)
(defstub midishare-receive)
(defstub midishare-receive-stop)
(defstub (special typeNote typeKeyOn typeKeyOff typeKeyPress
                  typeCtrlChange typeProgChange typeChanPress
                  typePitchWheel typePitchBend typeSongPos typeSongSel
                  typeClock typeStart typeContinue typeStop typeTune
                  typeActiveSens typeReset typeSysEx typeStream
                  typePrivate typeSeqNum typeTextual typeCopyright
                  typeSeqName typeInstrName typeLyric typeMarker
                  typeCuePoint typeChanPrefix typeEndTrack typeTempo
                  typeSMPTEOffset typePortPrefix typeKeySign
                  typeTimeSign))
;; player
(defstub OpenPlayer)
(defstub ClosePlayer)
(defstub MidiNewSeq)
(defstub MidiAddSeq)
(defstub StartPlayer)
(defstub ContPlayer)
(defstub StopPlayer)
(defstub PausePlayer)
(defstub GetAllTrackPlayer)
(defstub SetAllTrackPlayer)
(defstub GetTrackPlayer)
(defstub SetTrackPlayer)
(defstub SetParamPlayer)
(defstub SetTempoPlayer)
(defstub SetSynchroInPlayer)
(defstub MidiNewMidiFileInfos)
(defstub MidiFileLoad)
(defstub MidiFileSave)
(defstub mf-clicks)
(defstub mf-format)
(defstub mf-timedef)
(defstub MidiFreeMidiFileInfos)
(defstub MidiFreeSeq)
(defstub (special kMuteOn kMuteOff kSoloOn kSoloOff kMute kSolo
                           kExternalSync kInternalSync kClockSync
                           kSMPTESync TicksPerQuarterNote ))
)
|#

;;;
;;; The CM package definition.
;;;

(defpackage :cm
  (:shadow :make-load-form 
           ;; have to shadow these from cl package because they are
           ;; used as pattern class names which is "illegal" in
           ;; cltl2. their functions on the local symbol versions are
           ;; installed at the end of this file.
           ;; have to block these from CLM
           :io :ran :exit :quit :play :graph :control)
  (:use :common-lisp )
  ;; use keywords instead of strings for case sensitive lisps.
  (:import-from :cl-user #:cm #:use-system)
  (:import-from :clm 
                :mus-next
                :mus-bshort
                :mus-aifc
                :mus-riff
                :mus-lshort
                :*clm-with-sound-depth*
                :wsdat-play
                :init-with-sound
                :finish-with-sound
                :*clm-channels*
                :*clm-srate*
                :*clm-file-name*
                :clm-load
                :dac
                :*definstrument-hook*
                ;; these are also used by CM but defs don't conflict.
                ;#+(and clm2 (not clm3)) :graph 
                :spectrum :env :src :filter :delay
                )
  (:import-from :cmn 
                :init-clm-input
                :*exact-rhythms*
                :score
                :staff-descriptors
                :stfdat-staff
                :staff-data 
                :set-staff-number
                :set-staff-clef
                :finish-clm-input
                :find-staff
                :add-staff
                :add-data-1
                :add-note-to-staff)
  ;; (:import-from :midishare
  ;;       	:midishare :midiGetVersion :MidiOpen :MidiClose :MidiCountAppls
  ;;       	:MidiGetNamedAppl :MidiGetIndAppl :MidiErrIndex :MidiGetName
  ;;       	:MidiConnect :MidiGetTime :MidiIsConnected
  ;;       	:MidiSendIm :MidiSend :MidiSendAt :MidiAddSeq
  ;;       	:typeNote :typeKeyOn :typeKeyOff :typeKeyPress :typeCtrlChange
  ;;       	:typeProgChange :typeChanPress :typePitchWheel :typePitchBend
  ;;       	:typeSongPos :typeSongSel :typeClock :typeStart :typeContinue
  ;;       	:typeStop :typeTune :typeActiveSens :typeReset :typeSysEx
  ;;       	:typeStream :typePrivate :typeSeqNum :typeTextual
  ;;       	:typeCopyright :typeSeqName :typeInstrName :typeLyric
  ;;       	:typeMarker :typeCuePoint :typeChanPrefix :typeEndTrack
  ;;       	:typeTempo :typeSMPTEOffset :typePortPrefix :typeKeySign
  ;;       	:typeTimeSign :MidiNewEv :port :chan :field :bend :text :port
  ;;       	:ref :date :evtype :MidiCopyEv :MidiFreeEv :MidiAddField
  ;;               :midiTask :midiGetEv :MidiSetRcvAlarm
  ;;               :nullptr :nullptrp :MidiFlushEvs 
  ;;               ;;:MidiShareSync :MidiOpenSync :MidiCloseSync :MidiGetSyncEv
                
  ;;       	:OpenPlayer :ClosePlayer :midiNewSeq
  ;;       	:StartPlayer :ContPlayer :StopPlayer :PausePlayer
  ;;       	:kMuteOn :kMuteOff :kSoloOn :kSoloOff :kMute :kSolo
  ;;       	:kExternalSync :kInternalSync :kClockSync :kSMPTESync
  ;;       	:GetAllTrackPlayer :SetAllTrackPlayer
  ;;       	:GetTrackPlayer :SetTrackPlayer
  ;;       	:SetParamPlayer :SetTempoPlayer
  ;;       	:TicksPerQuarterNote
  ;;       	:SetSynchroInPlayer :MidiNewMidiFileInfos
  ;;       	:MidiFileLoad :MidiFileSave :mf-clicks :mf-format :mf-timedef)
  (:import-from :fomus :fomus :obj-partid :obj-id :part-events
                :event-base :event-off :event-note :event-dur :make-part
                :make-note :*parts* :part-opts
                :part :note :meas :timesig :keysig :fomus-file)
  (:export :accumulation :amplitude :append-object :audio-file :axis
           :*beat* :best-normal-form :between :cd :cents->scaler
           :chord :*chromatic-scale* :clm-file :cm :cm-version 
	   :cm-version-number :cmio
           :cmn :cmn-file :copier :copy-object :cycle :date-and-time
           :decimals :decode-interval :defaxis :defobject :defprocess
           :doeach :drunk :dumposc :eod? :eop? :events :expl :explseg
           :explsegs :f :false :find-object :fit :fm-spectrum
           :fold-objects :funcall :graph :harmonics :heap :hertz
	   :histogram :i :import-events :input :insert-object
	   :interp :interpl :interval :invert :io :join :keynum
	   :line :list-named-objects :list-subobjects :log-axis 
	   :lookup :*loudest* :make-cm :map-objects :map-pattern-data
           :map-subcontainers :map-subobjects :markov-analyze :markov
           :midi-chan-event :midi-channel-map :midi-channel-pressure
           :midi :midi-connections :midi-control-change :midi-eot
           :midi-file :midi-file-print :midi-key-pressure
           :midi-key-signature :midi-note-off :midi-note-on
           :midi-pitch-bend :midi-port-event :midi-program-change
           :midi-sequence-number :midi-sequencer-event
           :midi-smpte-offset :midi-stream :midi-system-event
           :midi-tempo-change :midi-text-event :midi-time-signature
           :midishare-stream :mode :new :next :note-accidental :note
           :note-name :now :object->cmn :object-name
           :object-parameters :object-time :octave-number :odds
           :output :palindrome :pattern-state :pattern-value :pattern?
           :pick :pickl :pitch-class :play :player-cont
           :player-load-midifile :player-mute :player-pause
           :player-save-midifile :player-set-tempo :player-solo
           :player-start :player-stop :player-stream :player-unmute
           :player-unsolo :plotter-add-layer :plotter-close
           :plotter-data :plotter :plotter-front-styling
           :plotter-property :plotter-redraw :plotter-scroll
           :plotter-zoom :point :power :prime-form :process :pval
           :pval :pwd :quantize :ran :range :ransegs :remove-object
	   :remove-subobjects :rescale-envelope :rescale :rewrite
	   :rewrite-generation :rhythm :rm-spectrum :rotation
	   :rts :rts-stop :rts-continue :rts? 
	   :recv :recv-set! :recv?
	   :save-object :sc-file :scale-max :scale-min
           :scale-mod :scale-order :*scale* :scale= :scale> :scale>=
           :scale< :scale<= :scaler->cents :sco-file :seq
           :set-clm-output-hook! :set-midi-output-hook!
           :set-sco-output-hook! :shell :shuffle :*softest* :sprout
           :stop :subcontainers :subobjects :sv :sv+ :sv* :*tempo*
           :tendency :thunk :*time-slots* :transpose :transposer :true
           :tuning :vary :wait :wait-until :weighting)
  )


;;;
;;; intern and export ms:new, ms:MidiPrintEv, ms:output ms:sprout
;;;

;; (in-package :cm)

;; ;; dynamically add a few symbols to breakout packages...
;; (flet ((addsyms (syms pkg)
;;          (map nil (lambda (s) (intern (symbol-name s) pkg)) syms)
;;          (export (mapcar #'(lambda (x) (find-symbol (symbol-name x) pkg))
;;                          syms)
;;                  pkg)))
;;   ;; (addsyms '(#:output #:now) :portmidi)
;;   ;; (addsyms '(#:new #:MidiPrintEv #:sprout #:output #:now
;;   ;;            #:midishare-receive #:midishare-receive-stop)
;;   ;;          :midishare)
;;   )
