;;; **********************************************************************
;;; Copyright (C) 2006 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

;;;
;;; Emacs/Slime support for Common Music. Most commands are not bound
;;; to keys, see entries for some suggestions. CM's documentation menu
;;; is installed under the SLIME -> Documentation submenu.
;;;
;;; M-x cm                                                       [Command]
;;;   Start up cm in a new frame, window, or buffer (see
;;;   cm-command and inferior-lisp-display below).
;;; M-x kill-cm                                                  [Command]
;;;   Kill existing *slime-repl* session.
;;; M-x enable-cm-commands                                       [Command]
;;;   Adds the following keyboard comands:
;;;   <f8>      One-stroke switching between repl and lisp buffer.
;;;   C-cC-dc   Find symbol at point in Common Music dictionary. 
;;;             (also in menu SLIME->Documentation->Common Music)
;;;   C-xC-e    Eval expr before, after, around or whole region.
;;;             On OS X this is also installed on APPLE-E.
;;;   TAB       Indent line, region or defun (if prefixed).
;;;
;;; *common-music-doc-root*                                     [Variable]
;;;   The root URL for browsing CM documentation. Defaults
;;;   to "http://commonmusic.sf.net/doc/"
;;; cm-program
;;;   The shell program to start CM. Defaults to "cm".
;;; cm-systems
;;;   A list of systems to load when CM starts.
;;; cm-scratch-mode
;;;   Emacs edit mode for *scratch* buffer, one of: lisp, sal or nil.

(unless (member 'slime features)
  (require 'slime)
  (slime-setup))

(when (member 'aquamacs features)
  (add-to-list 'obof-other-frame-regexps " \\*inferior-lisp\\*")
  (add-to-list 'obof-other-frame-regexps "\\*slime-repl\\\\*"))

;; update default value of inferior-lisp-program to "cm.sh"

(defvar cm-program
  (if (or (not (boundp 'inferior-lisp-program))
	  (not inferior-lisp-program)
	  (equal inferior-lisp-program "lisp"))
      (or (locate-library "bin/cm.sh" t)
	  "cm")))

(defvar cm-systems (list))

(defvar cm-scratch-mode 'lisp)

;; add music extensions if not already present...
(loop for mode in '(("\\.clm$" . lisp-mode)
		    ("\\.cm$"  . lisp-mode)
		    ("\\.cmn$" . lisp-mode)
		    ("\\.ins$" . lisp-mode)
		    ("\\.fms$" . lisp-mode)
		    ("\\.asd$" . lisp-mode))
      do (add-to-list 'auto-mode-alist mode))

;; add music-related ignored file types...
(loop for type in '(".midi" ".mid" ".snd" ".aiff" ".wav" ".osc"
		    ".fas" ".dfas" ".fasl" ".lib" ".ppcf" ".so"
		    ".dylib")
      do (add-to-list 'completion-ignored-extensions type))

;; set lisp buffers to slime mode...
(add-hook 'inferior-lisp-mode-hook
            (lambda ()
	      (slime-mode t)
	      (setq indent-tabs-mode nil)
	      ))

;; connect hook executes (cm) to set readtable etc and then removes
;; itself so that it doesnt interfere with other slime sessions.

(defun cm-start-hook ()
  ;;(slime-repl-send-string "(cm)")
  (slime-interactive-eval "(cm)")
  (remove-hook 'slime-connected-hook 'cm-start-hook)
  ;; aquamacs: hide inferior lisp buffer if visible after slime buffer
  ;; starts. This happens if user re-mouses original frame after doing
  ;; M-x cm before slime repl has been activated. if user then closes
  ;; the visible inferior-lisp frame the lisp session is hosed.
  ;; (when (member 'aquamacs features)
  ;;    (replace-buffer-in-windows (get-buffer "*inferior-lisp*")))
  (when (member 'aquamacs features)
    (let ((ilw (get-buffer-window " *inferior-lisp*" t)))
      (if ilw (delete-frame (window-frame ilw))))
    ))

;; Darwin: define COMMAND-E to evaluate expr a la MCL.
(if (equal system-type 'darwin)
    (global-set-key [(alt e)] 'slime-eval-expr))

;; add cm startup actions to inferior-lisp startup BEFORE repl has
;; been established

(defun cm-init-command (port coding)
  ;; get slime's inits 
  (let ((init (slime-init-command port coding)))
    ;; append system loading before repl bufer starts
    (dolist (s cm-systems)
      (setq init
	    (concat init (if (keywordp s)
			     (format "(use-system %s)\n" s)
			   (format "(use-system :%s)\n" s)))))
    init))

(defun cm (program )
  "Start CM"
  (interactive (list (if prefix-arg
			 (read-string "Command to start CM: " "cm") 
		       nil)))
  (cond ((slime-connected-p)
	 (switch-to-buffer (slime-repl-buffer)))
	(t
	 (when program (setq cm-program program))
	 (let ((parsed (split-string cm-program)))
	   (add-hook 'slime-connected-hook 'cm-start-hook)
	   (slime-start :program (first parsed) :program-args (rest parsed)
			:init 'cm-init-command
			:buffer " *inferior-lisp*"
			)
	   (claim-scratch-buffer)))))

(defun kill-cm ()
  "Kill *slime-repl* and all associated buffers."
  (interactive)
  (slime-repl-sayoonara))

(defun enable-cm-commands ()
  (interactive )
  ;; 1 stroke switching between repl and last editing buffer
  (global-set-key (kbd "<f8>") 'slime-toggle-repl)
  ;; eval before at or after point, region, or whole defun on whitespae
  (define-key slime-mode-map (kbd "\C-x\C-e") 'slime-eval-expr)
  ;; indent line or region
  (define-key slime-mode-map (kbd "TAB") 'slime-indent-anything)
  ;; lookup cm function at point
  (define-key slime-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
  )

(defun slime-toggle-repl ()
  "Toggle between *slime-repl* and last lisp or SAL buffer."
  (interactive)
  (if (slime-connected-p)
      (let ((repl (slime-repl-buffer)))
        (if repl
            (let ((this (current-buffer))
		  next)
              (if (eq repl this)
                  (setq next (loop for b in (buffer-list)
				   when (with-current-buffer b
					  (or (eq major-mode 'lisp-mode)
					      (eq major-mode 'sal-mode)
					      ))
				   return b))
		(setq next (slime-repl-buffer)))
	      (when next
		;;(pop-to-buffer next)
		;;(switch-to-buffer-other-frame next)
		(switch-to-buffer next)))))))

(defun claim-scratch-buffer ()
  ;; if scratch buffer is empty set to slime or SAL mode
  (let ((scratch (get-buffer "*scratch*")))
    (if scratch
	(if (not (buffer-modified-p scratch))
	    (with-current-buffer scratch
	      (cond ((equal cm-scratch-mode 'lisp)
		     (lisp-mode)
		     (setq slime-buffer-package "cm")
		     (insert (format "(in-package :cm)\n\n"))
		     (goto-char (point-max)))
		    ((equal cm-scratch-mode 'sal)
		     (sal-mode)
		     (insert (format "; Use this buffer for SAL commands.\n\n"))
		     (goto-char (point-max)))
		    (t )))))))

(when (not (featurep 'xemacs))
  (defun region-exists-p ()
    (and mark-active ; simple.el
	 (not (null (mark))))))

(defun slime-eval-expr ()
  "Evals expr before point, at point, around point, whole region."
  (interactive)
  (if (region-exists-p )
      (slime-eval-region (region-beginning) (region-end))
    (let ((wspace '(?\  ?\t ?\r ?\n))
	  (left-char (char-before))
	  (right-char (char-after))
	  left-side right-side)
      (setq left-side
	    (if (or (not left-char)
		    (member left-char wspace)
		    (member left-char '(?\( )))
		(point)
	      (save-excursion
		(backward-sexp)
		(point))))
      (setq right-side
	    (if (or (not right-char)
		    (member right-char wspace)
		    (member right-char '(?\) ))
		    ;; dont look ahead if different sexp leftward
		    (and (< left-side (point))
			 (char-equal left-char ?\))))
		(point)
	      (save-excursion
		(forward-sexp)
		(point))))
      (if (equal left-side right-side)   
	  nil
	(slime-interactive-eval
	 (buffer-substring-no-properties left-side right-side))))))

(defun slime-indent-anything ()
  "Do line indentation/symbol completion; indent region if
selected; indent whole defun if prefixed."
  (interactive)
  (if current-prefix-arg
      (slime-reindent-defun )
    (if (and (region-exists-p)
	     (> (count-lines (region-beginning) (region-end)) 1))
	(lisp-indent-region (region-beginning) (region-end))
      (slime-indent-and-complete-symbol))))

;;;
;;; CM documentation hacks, mostly cribbed from hyperspec.
;;;

(defvar *common-music-doc-root* "http://commonmusic.sf.net/doc/"
  "The root url for visiting CM documentation.")

(defun cm-doc (url)
  (interactive "FCM document:")
  (browse-url (concatenate 'string *common-music-doc-root*
			   url)))

(defun cm-lookup (entry)
  (interactive (list
		(let* ((it (thing-at-point 'symbol))
		       (sy (and it (downcase it))))
		  (if (and sy (intern-soft sy *common-music-symbols*))
		      sy
		    (completing-read "Lookup CM symbol: "
				     *common-music-symbols*
				     #'boundp t nil nil nil)))))
  (if entry
      (let ((sym (intern-soft (downcase entry) *common-music-symbols*)))
	(if (and sym (boundp sym))
	    (cm-doc (car (symbol-value sym)))))))

(defvar *common-music-doc-menu*
  `("Common Music"
    [ "Home Page" (cm-doc "cm.html")]
    [ "Installing" (cm-doc "install.html")]
    [ "Working in Emacs" (cm-doc "emacs.html")]
    "--"
    [ "Tutorials" (cm-doc "../etc/tutorials/")]
    [ "Examples"  (cm-doc "../etc/examples/")]
    "--"
    [ "Dictionary" (cm-doc "dict/index.html") ]
    [ "Lookup..." cm-lookup ]
    ))

;; add Common Music documentation menu to Slime...
;; last argument changed to make it workable under xemacs 21.4.19
;; Robert Matovinovic, 20.07.2006, robert.matovinovic@web.de

(easy-menu-add-item menubar-slime
		    '("Documentation")
		    *common-music-doc-menu*
		    ;(easy-menu-create-menu "Common Music" )
		    )

(defvar *common-music-symbols* (make-vector 66 0))

(mapcar
 (lambda (entry)
   (let ((symbol (intern (car entry)
			 *common-music-symbols*)))
     (if (boundp symbol)
	 (push (cadr entry) (symbol-value symbol))
       (set symbol (cdr entry)))))
 ;; *** generate by loading "/Lisp/cm/doc/dict/index.lisp"
 '(
; warning: /Lisp/cm/doc/dict/osc-stream-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/dict/sc-file-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/dict/sc-stream-cls.html has no <title> tag.
   ("*beat*" "dict/beat-var.html")
   ("*chromatic-scale*" "dict/chromatic-scale-var.html")
   ("*loudest*" "dict/loudest-var.html")
   ("*midi-channel-map*" "dict/midi-channel-map-var.html")
   ("*midi-connections*" "dict/midi-connections-var.html")
   ("*portmidi-default-filter*" "dict/portmidi-topic.html#*portmidi-default-filter*")
   ("*portmidi-default-inbuf-size*" "dict/portmidi-topic.html#*portmidi-default-inbuf-size*")
   ("*portmidi-default-input*" "dict/portmidi-topic.html#*portmidi-default-input*")
   ("*portmidi-default-latency*" "dict/portmidi-topic.html#*portmidi-default-latency*")
   ("*portmidi-default-mask*" "dict/portmidi-topic.html#*portmidi-default-mask*")
   ("*portmidi-default-outbuf-size*" "dict/portmidi-topic.html#*portmidi-default-outbuf-size*")
   ("*portmidi-default-output*" "dict/portmidi-topic.html#*portmidi-default-output*")
   ("*power*" "dict/power-var.html")
   ("*scale*" "dict/scale-var.html")
   ("*softest*" "dict/softest-var.html")
   ("*tempo*" "dict/tempo-var.html")
   ("accumulation" "dict/accumulation-cls.html")
   ("active-sensing-p" "dict/midi-topic.html#active-sensing-p")
   ("active-sensing-route" "dict/midi-topic.html#active-sensing-route")
   ("amplitude" "dict/amplitude-fn.html")
   ("append-object" "dict/append-object-fn.html")
   ("audio-file" "dict/audio-file-cls.html")
   ("axis" "dict/axis-fn.html")
   ("axis" "dict/axis-cls.html")
   ("between" "dict/between-fn.html")
   ("cable-select-cable" "dict/midi-topic.html#cable-select-cable")
   ("cable-select-p" "dict/midi-topic.html#cable-select-p")
   ("cable-select-route" "dict/midi-topic.html#cable-select-route")
   ("cd" "dict/cd-fn.html")
   ("cents->scaler" "dict/cents-gtscaler-fn.html")
   ("channel-message-channel" "dict/midi-topic.html#channel-message-channel")
   ("channel-message-data1" "dict/midi-topic.html#channel-message-data1")
   ("channel-message-data2" "dict/midi-topic.html#channel-message-data2")
   ("channel-message-opcode" "dict/midi-topic.html#channel-message-opcode")
   ("channel-message-p" "dict/midi-topic.html#channel-message-p")
   ("channel-pressure-channel" "dict/midi-topic.html#channel-pressure-channel")
   ("channel-pressure-p" "dict/midi-topic.html#channel-pressure-p")
   ("channel-pressure-pressure" "dict/midi-topic.html#channel-pressure-pressure")
   ("chord" "dict/chord-cls.html")
   ("clm-file" "dict/clm-file-cls.html")
   ("cm-version" "dict/cm-version-fn.html")
   ("cm.sh" "dict/cm-sh.html")
   ("cmio" "dict/cmio-fn.html")
   ("cmn" "dict/cmn-cls.html")
   ("cmn-file" "dict/cmn-file-cls.html")
   ("continue-p" "dict/midi-topic.html#continue-p")
   ("continue-route" "dict/midi-topic.html#continue-route")
   ("control-change-channel" "dict/midi-topic.html#control-change-channel")
   ("control-change-controller" "dict/midi-topic.html#control-change-controller")
   ("control-change-p" "dict/midi-topic.html#control-change-p")
   ("control-change-value" "dict/midi-topic.html#control-change-value")
   ("copier" "dict/copier-cls.html")
   ("copy-object" "dict/copy-object-fn.html")
   ("copyright-note-p" "dict/midi-topic.html#copyright-note-p")
   ("cue-point-p" "dict/midi-topic.html#cue-point-p")
   ("cycle" "dict/cycle-cls.html")
   ("date-and-time" "dict/date-and-time-fn.html")
   ("decimals" "dict/decimals-fn.html")
   ("decode-interval" "dict/decode-interval-fn.html")
   ("defaxis" "dict/defaxis-mac.html")
   ("defobject" "dict/defobject-mac.html")
   ("doeach" "dict/doeach-mac.html")
   ("drunk" "dict/drunk-fn.html")
   ("dumposc" "dict/dumposc-fn.html")
   ("eod?" "dict/eodqmk-fn.html")
   ("eop?" "dict/eopqmk-fn.html")
   ("eot-p" "dict/midi-topic.html#eot-p")
   ("eox-p" "dict/midi-topic.html#eox-p")
   ("eox-route" "dict/midi-topic.html#eox-route")
   ("events" "dict/events-fn.html")
   ("expl" "dict/expl-fn.html")
   ("explseg" "dict/explseg-fn.html")
   ("explsegs" "dict/explsegs-fn.html")
   ("f" "dict/f-cls.html")
   ("false" "dict/false-var.html")
   ("find-object" "dict/find-object-fn.html")
   ("fit" "dict/fit-fn.html")
   ("fm-spectrum" "dict/fm-spectrum-fn.html")
   ("fold-objects" "dict/fold-objects-fn.html")
   ("fomus-file" "dict/fomus-file-cls.html")
   ("graph" "dict/graph-cls.html")
   ("harmonics" "dict/harmonics-fn.html")
   ("heap" "dict/heap-cls.html")
   ("hertz" "dict/hertz-fn.html")
   ("histogram" "dict/histogram-fn.html")
   ("i" "dict/i-cls.html")
   ("import-events" "dict/import-events-fn.html")
   ("input" "dict/input-fn.html")
   ("insert-object" "dict/insert-object-fn.html")
   ("instrument-name-p" "dict/midi-topic.html#instrument-name-p")
   ("interp" "dict/interp-fn.html")
   ("interpl" "dict/interpl-fn.html")
   ("interval" "dict/interval-fn.html")
   ("invert" "dict/invert-fn.html")
   ("io" "dict/io-mac.html")
   ("join" "dict/join-cls.html")
   ("key-pressure-channel" "dict/midi-topic.html#key-pressure-channel")
   ("key-pressure-key" "dict/midi-topic.html#key-pressure-key")
   ("key-pressure-p" "dict/midi-topic.html#key-pressure-p")
   ("key-pressure-pressure" "dict/midi-topic.html#key-pressure-pressure")
   ("key-signature-p" "dict/midi-topic.html#key-signature-p")
   ("keynum" "dict/keynum-fn.html")
   ("line" "dict/line-cls.html")
   ("list-named-objects" "dict/list-named-objects-fn.html")
   ("list-objects" "dict/list-objects-fn.html")
   ("log-axis" "dict/log-axis-cls.html")
   ("lookup" "dict/lookup-fn.html")
   ("lyric-p" "dict/midi-topic.html#lyric-p")
   ("make-active-sensing" "dict/midi-topic.html#make-active-sensing")
   ("make-cable-select" "dict/midi-topic.html#make-cable-select")
   ("make-channel-message" "dict/midi-topic.html#make-channel-message")
   ("make-channel-pressure" "dict/midi-topic.html#make-channel-pressure")
   ("make-cm" "dict/make-cm-fn.html")
   ("make-continue" "dict/midi-topic.html#make-continue")
   ("make-control-change" "dict/midi-topic.html#make-control-change")
   ("make-copyright-note" "dict/midi-topic.html#make-copyright-note")
   ("make-cue-point" "dict/midi-topic.html#make-cue-point")
   ("make-eot" "dict/midi-topic.html#make-eot")
   ("make-eox" "dict/midi-topic.html#make-eox")
   ("make-instrument-name" "dict/midi-topic.html#make-instrument-name")
   ("make-key-pressure" "dict/midi-topic.html#make-key-pressure")
   ("make-key-signature" "dict/midi-topic.html#make-key-signature")
   ("make-lyric" "dict/midi-topic.html#make-lyric")
   ("make-marker" "dict/midi-topic.html#make-marker")
   ("make-meta-message" "dict/midi-topic.html#make-meta-message")
   ("make-midi-channel" "dict/midi-topic.html#make-midi-channel")
   ("make-midi-port" "dict/midi-topic.html#make-midi-port")
   ("make-mtc-quarter-frame" "dict/midi-topic.html#make-mtc-quarter-frame")
   ("make-note-off" "dict/midi-topic.html#make-note-off")
   ("make-note-on" "dict/midi-topic.html#make-note-on")
   ("make-pitch-bend" "dict/midi-topic.html#make-pitch-bend")
   ("make-program-change" "dict/midi-topic.html#make-program-change")
   ("make-sequence-number" "dict/midi-topic.html#make-sequence-number")
   ("make-sequencer-event" "dict/midi-topic.html#make-sequencer-event")
   ("make-sequence_track-name" "dict/midi-topic.html#make-sequence_track-name")
   ("make-smpte-offset" "dict/midi-topic.html#make-smpte-offset")
   ("make-song-position" "dict/midi-topic.html#make-song-position")
   ("make-song-select" "dict/midi-topic.html#make-song-select")
   ("make-start" "dict/midi-topic.html#make-start")
   ("make-stop" "dict/midi-topic.html#make-stop")
   ("make-sysex" "dict/midi-topic.html#make-sysex")
   ("make-system-message" "dict/midi-topic.html#make-system-message")
   ("make-system-reset" "dict/midi-topic.html#make-system-reset")
   ("make-tempo-change" "dict/midi-topic.html#make-tempo-change")
   ("make-text-event" "dict/midi-topic.html#make-text-event")
   ("make-time-signature" "dict/midi-topic.html#make-time-signature")
   ("make-timing-clock" "dict/midi-topic.html#make-timing-clock")
   ("make-timing-tick" "dict/midi-topic.html#make-timing-tick")
   ("make-tune-request" "dict/midi-topic.html#make-tune-request")
   ("map-objects" "dict/map-objects-fn.html")
   ("map-pattern-data" "dict/map-pattern-data-fn.html")
   ("map-subcontainers" "dict/map-subcontainers-fn.html")
   ("map-subobjects" "dict/map-subobjects-fn.html")
   ("marker-p" "dict/midi-topic.html#marker-p")
   ("markov" "dict/markov-cls.html")
   ("markov-analyze" "dict/markov-analyze-fn.html")
   ("meta-message-p" "dict/midi-topic.html#meta-message-p")
   ("meta-message-type" "dict/midi-topic.html#meta-message-type")
   ("midi" "dict/midi-cls.html")
   ("midi-chan-event" "dict/midi-chan-event-cls.html")
   ("midi-channel-p" "dict/midi-topic.html#midi-channel-p")
   ("midi-channel-pressure" "dict/midi-channel-pressure-cls.html")
   ("midi-control-change" "dict/midi-control-change-cls.html")
   ("midi-copy-message" "dict/midi-topic.html#midi-copy-message")
   ("midi-eot" "dict/midi-eot-cls.html")
   ("midi-file" "dict/midi-file-cls.html")
   ("midi-file-print" "dict/midi-file-print-fn.html")
   ("midi-key-pressure" "dict/midi-key-pressure-cls.html")
   ("midi-key-signature" "dict/midi-key-signature-cls.html")
   ("midi-note-off" "dict/midi-note-off-cls.html")
   ("midi-note-on" "dict/midi-note-on-cls.html")
   ("midi-pitch-bend" "dict/midi-pitch-bend-cls.html")
   ("midi-port-event" "dict/midi-port-event-cls.html")
   ("midi-port-p" "dict/midi-topic.html#midi-port-p")
   ("midi-print-message" "dict/midi-topic.html#midi-print-message")
   ("midi-program-change" "dict/midi-program-change-cls.html")
   ("midi-sequence-number" "dict/midi-sequence-number-cls.html")
   ("midi-sequencer-event" "dict/midi-sequencer-event-cls.html")
   ("midi-smpte-offset" "dict/midi-smpte-offset-cls.html")
   ("midi-stream" "dict/midi-stream-cls.html")
   ("midi-system-event" "dict/midi-system-event-cls.html")
   ("midi-tempo-change" "dict/midi-tempo-change-cls.html")
   ("midi-text-event" "dict/midi-text-event-cls.html")
   ("midi-time-signature" "dict/midi-time-signature-cls.html")
   ("midishare-open" "dict/midishare-topic.html#midishare-open")
   ("midishare-open?" "dict/midishare-topic.html#midishare-open?")
   ("mode" "dict/mode-cls.html")
   ("ms:midiprintev" "dict/midishare-topic.html#ms:midiprintev")
   ("ms:new" "dict/midishare-topic.html#ms:new")
   ("mtc-quarter-frame-nibble" "dict/midi-topic.html#mtc-quarter-frame-nibble")
   ("mtc-quarter-frame-p" "dict/midi-topic.html#mtc-quarter-frame-p")
   ("mtc-quarter-frame-route" "dict/midi-topic.html#mtc-quarter-frame-route")
   ("mtc-quarter-frame-tag" "dict/midi-topic.html#mtc-quarter-frame-tag")
   ("new" "dict/new-mac.html")
   ("next" "dict/next-fn.html")
   ("note" "dict/note-fn.html")
   ("note-accidental" "dict/note-accidental-fn.html")
   ("note-name" "dict/note-name-fn.html")
   ("note-off-channel" "dict/midi-topic.html#note-off-channel")
   ("note-off-key" "dict/midi-topic.html#note-off-key")
   ("note-off-p" "dict/midi-topic.html#note-off-p")
   ("note-off-velocity" "dict/midi-topic.html#note-off-velocity")
   ("note-on-channel" "dict/midi-topic.html#note-on-channel")
   ("note-on-key" "dict/midi-topic.html#note-on-key")
   ("note-on-p" "dict/midi-topic.html#note-on-p")
   ("note-on-velocity" "dict/midi-topic.html#note-on-velocity")
   ("now" "dict/now-fn.html")
   ("object->cmn" "dict/object-gtcmn-fn.html")
   ("object-name" "dict/object-name-fn.html")
   ("object-parameters" "dict/object-parameters-fn.html")
   ("object-time" "dict/object-time-fn.html")
   ("octave-number" "dict/octave-number-fn.html")
   ("odds" "dict/odds-fn.html")
   ("output" "dict/output-fn.html")
   ("palindrome" "dict/palindrome-cls.html")
   ("pattern-state" "dict/pattern-state-fn.html")
   ("pattern-value" "dict/pattern-value-fn.html")
   ("pattern?" "dict/patternqmk-fn.html")
   ("pick" "dict/pick-fn.html")
   ("pickl" "dict/pickl-fn.html")
   ("pitch-bend-channel" "dict/midi-topic.html#pitch-bend-channel")
   ("pitch-bend-lsb" "dict/midi-topic.html#pitch-bend-lsb")
   ("pitch-bend-msb" "dict/midi-topic.html#pitch-bend-msb")
   ("pitch-bend-p" "dict/midi-topic.html#pitch-bend-p")
   ("pitch-class" "dict/pitch-class-fn.html")
   ("play" "dict/play-fn.html")
   ("plotter" "dict/plotter-fn.html")
   ("plotter-add-layer" "dict/plotter-add-layer-fn.html")
   ("plotter-data" "dict/plotter-data-fn.html")
   ("plotter-front-styling" "dict/plotter-front-styling-fn.html")
   ("plotter-property" "dict/plotter-property-fn.html")
   ("plotter-redraw" "dict/plotter-redraw-fn.html")
   ("plotter-redraw" "dict/plotter-close-fn.html")
   ("plotter-scroll" "dict/plotter-scroll-fn.html")
   ("plotter-zoom" "dict/plotter-zoom-fn.html")
   ("pm:countdevices" "dict/portmidi-topic.html#pm:countdevices")
   ("pm:getdefaultinputdeviceid" "dict/portmidi-topic.html#pm:getdefaultinputdeviceid")
   ("pm:getdefaultoutputdeviceid" "dict/portmidi-topic.html#pm:getdefaultoutputdeviceid")
   ("pm:getdeviceinfo" "dict/portmidi-topic.html#pm:getdeviceinfo")
   ("pm:time" "dict/portmidi-topic.html#pm:time")
   ("point" "dict/point-cls.html")
   ("portmidi-close" "dict/portmidi-topic.html#portmidi-close")
   ("portmidi-open" "dict/portmidi-topic.html#portmidi-open")
   ("portmidi-open?" "dict/portmidi-topic.html#portmidi-open?")
   ("portmidi-record!" "dict/portmidi-topic.html#portmidi-record")
   ("portmidi-stream" "dict/portmidi-topic.html#portmidi-stream")
   ("prime-form" "dict/prime-form-fn.html")
   ("process" "dict/process-mac.html")
   ("program-change-channel" "dict/midi-topic.html#program-change-channel")
   ("program-change-p" "dict/midi-topic.html#program-change-p")
   ("program-change-program" "dict/midi-topic.html#program-change-program")
   ("pval" "dict/pval-mac.html")
   ("pval" "dict/pval-cls.html")
   ("pwd" "dict/pwd-fn.html")
   ("quantize" "dict/quantize-fn.html")
   ("ran" "dict/ran-fn.html")
   ("range" "dict/range-cls.html")
   ("ransegs" "dict/ransegs-fn.html")
   ("receive" "dict/receive-fn.html")
   ("receiver?" "dict/receiverqmk-fn.html")
   ("remove-object" "dict/remove-object-fn.html")
   ("remove-receiver!" "dict/remove-receiver-fn.html")
   ("remove-subobjects" "dict/remove-subobjects-fn.html")
   ("rescale" "dict/rescale-fn.html")
   ("rescale-envelope" "dict/rescale-envelope-fn.html")
   ("rewrite" "dict/rewrite-cls.html")
   ("rewrite-generation" "dict/rewrite-generation-fn.html")
   ("rhythm" "dict/rhythm-fn.html")
   ("rm-spectrum" "dict/rm-spectrum-fn.html")
   ("rotation" "dict/rotation-cls.html")
   ("rts" "dict/rts-fn.html")
   ("rts-continue" "dict/rts-continue-fn.html")
   ("rts-pause" "dict/rts-pause-fn.html")
   ("rts-stop" "dict/rts-stop-fn.html")
   ("rts?" "dict/rtsqmk-fn.html")
   ("save-object" "dict/save-object-fn.html")
   ("sc-clearsched" "dict/supercollider-topic.html#sc-clearsched")
   ("sc-close" "dict/supercollider-topic.html#sc-close")
   ("sc-dumposc" "dict/supercollider-topic.html#sc-dumposc")
   ("sc-flush" "dict/supercollider-topic.html#sc-flush")
   ("sc-notify" "dict/supercollider-topic.html#sc-notify")
   ("sc-open" "dict/supercollider-topic.html#sc-open")
   ("sc-open?" "dict/supercollider-topic.html#sc-open?")
   ("sc-quit" "dict/supercollider-topic.html#sc-quit")
   ("scale-max" "dict/scale-max-fn.html")
   ("scale-min" "dict/scale-min-fn.html")
   ("scale-mod" "dict/scale-mod-fn.html")
   ("scale-order" "dict/scale-order-fn.html")
   ("scale<" "dict/scalelt-fn.html")
   ("scale<=" "dict/scalelteql-fn.html")
   ("scale=" "dict/scaleeql-fn.html")
   ("scale>" "dict/scalegt-fn.html")
   ("scale>=" "dict/scalegteql-fn.html")
   ("scaler->cents" "dict/scaler-gtcents-fn.html")
   ("sco-file" "dict/sco-file-cls.html")
   ("seq" "dict/seq-cls.html")
   ("sequence-number-p" "dict/midi-topic.html#sequence-number-p")
   ("sequencer-event-p" "dict/midi-topic.html#sequencer-event-p")
   ("sequence_track-name-p" "dict/midi-topic.html#sequence_track-name-p")
   ("set-clm-output-hook!" "dict/set-clm-output-hook-fn.html")
   ("set-midi-output-hook!" "dict/set-midi-output-hook-fn.html")
   ("set-receiver!" "dict/set-receiver-fn.html")
   ("set-sco-output-hook!" "dict/set-sco-output-hook-fn.html")
   ("shell" "dict/shell-fn.html")
   ("shuffle" "dict/shuffle-fn.html")
   ("smpte-offset-p" "dict/midi-topic.html#smpte-offset-p")
   ("song-position-lsb" "dict/midi-topic.html#song-position-lsb")
   ("song-position-msb" "dict/midi-topic.html#song-position-msb")
   ("song-position-p" "dict/midi-topic.html#song-position-p")
   ("song-position-route" "dict/midi-topic.html#song-position-route")
   ("song-select-p" "dict/midi-topic.html#song-select-p")
   ("song-select-route" "dict/midi-topic.html#song-select-route")
   ("song-select-song" "dict/midi-topic.html#song-select-song")
   ("sprout" "dict/sprout-fn.html")
   ("start-p" "dict/midi-topic.html#start-p")
   ("start-route" "dict/midi-topic.html#start-route")
   ("stop" "dict/stop-fn.html")
   ("stop-p" "dict/midi-topic.html#stop-p")
   ("stop-route" "dict/midi-topic.html#stop-route")
   ("subcontainers" "dict/subcontainers-fn.html")
   ("subobjects" "dict/subobjects-fn.html")
   ("sv" "dict/sv-mac.html")
   ("sv*" "dict/svstar-mac.html")
   ("sv+" "dict/svplus-mac.html")
   ("sysex-p" "dict/midi-topic.html#sysex-p")
   ("sysex-route" "dict/midi-topic.html#sysex-route")
   ("system-message-data1" "dict/midi-topic.html#system-message-data1")
   ("system-message-data2" "dict/midi-topic.html#system-message-data2")
   ("system-message-p" "dict/midi-topic.html#system-message-p")
   ("system-message-route" "dict/midi-topic.html#system-message-route")
   ("system-message-status" "dict/midi-topic.html#system-message-status")
   ("system-reset-p" "dict/midi-topic.html#system-reset-p")
   ("system-reset-route" "dict/midi-topic.html#system-reset-route")
   ("tempo-change-p" "dict/midi-topic.html#tempo-change-p")
   ("tendency" "dict/tendency-fn.html")
   ("text-event-p" "dict/midi-topic.html#text-event-p")
   ("thunk" "dict/thunk-cls.html")
   ("time-signature-p" "dict/midi-topic.html#time-signature-p")
   ("timing-clock-p" "dict/midi-topic.html#timing-clock-p")
   ("timing-clock-route" "dict/midi-topic.html#timing-clock-route")
   ("timing-tick-p" "dict/midi-topic.html#timing-tick-p")
   ("timing-tick-route" "dict/midi-topic.html#timing-tick-route")
   ("transpose" "dict/transpose-fn.html")
   ("transposer" "dict/transposer-cls.html")
   ("true" "dict/true-var.html")
   ("tune-request-p" "dict/midi-topic.html#tune-request-p")
   ("tune-request-route" "dict/midi-topic.html#tune-request-route")
   ("tuning" "dict/tuning-cls.html")
   ("use-system" "dict/use-system-fn.html")
   ("vary" "dict/vary-fn.html")
   ("wait" "dict/wait-fn.html")
   ("wait-until" "dict/wait-until-fn.html")
   ("weighting" "dict/weighting-cls.html"))
 )

;; eof




