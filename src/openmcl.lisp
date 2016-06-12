;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name$
;;; $Revision: 1033 $
;;; $Date: 2006-03-24 17:15:42 +0100 (Fri, 24 Mar 2006) $

(in-package :cm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl::open-shared-library "Carbon.framework/Carbon")
  (ccl::use-interface-dir :carbon)
  (require "pascal-strings")
  )

(pushnew :metaclasses *features*)

(import '(ccl:slot-definition-name 
          ccl:slot-definition-initargs 
          ccl:slot-definition-initform 
          ccl:class-direct-superclasses
          ccl:class-direct-subclasses
          ccl:open-shared-library ; needed if loading clm into cm.
          ccl:without-interrupts ; for testing realtime
          #+:openmcl-partial-mop
          ccl:class-slots
          #+:openmcl-partial-mop
          ccl:class-direct-slots
          #+:openmcl-partial-mop
          ccl:validate-superclass
          ))

#-:openmcl-partial-mop
(progn
  (defun class-slots (class) 
    (ccl::class-instance-slots class))
  (defun class-direct-slots (class)
    (ccl:class-direct-instance-slots class))
  (defmethod validate-class ((class t) (superclass t))
    ;; this is a no-op except in OpenMCL 014
    t)
  )

(defun finalize-class (class) class t)

(defmethod make-load-form (obj) (cl:make-load-form obj))

(defun slot-definition-reader (slot) slot nil)

;;;
;;; misc stuff
;;;

(defun quit () (ccl:quit))

(defun exit () (quit))

(defun object-address (x)
  (ccl:%address-of x))

(defun generic-function-name (fn)
  (ccl::function-name fn))

;(defun cd (&optional dir)
;  (if (null dir)
;    (namestring (ccl::mac-default-directory))
;    (ccl::cwd dir)))

(defun cd (&optional (dir (user-homedir-pathname )))
  (ccl::cwd dir))

(defun pwd ()
  (namestring (ccl::mac-default-directory)))

(defun explode-string (str)
  ;; parse str into a list of tokens
  ;; delimited by whitespace
  (let ((white '(#\space #\tab))
	(slen (length str))
	(args '()))

    (loop with i = 0 and b and s and l
	  while (< i slen)
	  do
	  ;; flush whitespace
	  (loop while (and (< i slen)
			   (member (elt str i) white))
	    do (incf i))
	  (unless (< i slen)
	    (return))
	  (setf b i)
	  (setf s nil)
	  (setf l #\null)
	  ;; read until next undelimited whitspace
	  (loop while (and (< i slen)
			   (or (not (member (elt str i) white))
			       (char= l #\\)
			       s))
	    do
	    (if (char= (elt str i) #\")
	      (setf s (not s)))
	    (setf l (elt str i))
	    (incf i))
	  (push (subseq str b i) args))
    (nreverse args)))

(defun shell (cmd &key (output t) (wait t))
  (ccl:run-program "/bin/csh" (list "-fc" cmd)
                   :output output :wait wait))

(defparameter *browser* nil)

(defun open-url (url &key (browser *browser*))
  (ccl:run-program "open" (list "-a" browser url))
  (values))

(defconstant directory-delimiter #\/)

(defun env-var (var)
  (ccl::getenv (string var)))

(defun set-env-var (var val)
  (ccl::setenv (string var) val))

;;;
;;; cm application classes
;;; 

(defclass cm-application (ccl::lisp-development-system) ())
(defclass cm-carbon-application (cm-application) ())

(defparameter *cm-application-class* (find-class 'cm-application))

(defmethod initialize-instance :after ((obj cm-application) &rest args)
  args
  (setf (slot-value obj 'ccl::command-line-arguments)
	(list ccl::*standard-help-argument*
	      (ccl::make-command-line-argument
	       :option-char #\I
	       :long-name "image-name"
	       :keyword :image-name
	       :help-string "image-name <file>"
	       :may-take-operand t
	       :allow-multiple nil)
	      (ccl::make-command-line-argument
	       :option-char #\l
	       :long-name "load"
	       :keyword :load
	       :help-string "load <file>"
	       :may-take-operand t
	       :allow-multiple t)
	      (ccl::make-command-line-argument
	       :option-char #\e
	       :long-name "eval"
	       :keyword :eval
	       :help-string "evaluate <form> (may need to quote <form> in shell)"
	       :may-take-operand t
	       :allow-multiple t))))

(defmethod ccl:application-name ((app cm-application)) "Common Music")

(defmethod ccl::application-version-string ((a cm-application))
  (cm-version))

(defmethod ccl:toplevel-function ((app cm-application) init-file)
  (declare (ignore init-file))
  (call-next-method))

(defparameter *cm-swank-port* nil)

(defmethod ccl:toplevel-function ((app cm-carbon-application) init-file)
  (declare (ignore init-file) (special *cm-readtable*))
;;  (setf *package* (find-package :cm))
;;  (setf *readtable* *cm-readtable*)
;;  (load-cminit)
;;  (cm-logo)
  (when (and *cm-swank-port* (find-package ':swank))
    (funcall (find-symbol "CREATE-SERVER" ':swank)
             :port *cm-swank-port*))
  (ccl::with-pstrs ((msg "Hello from CM!"))
    (#_StandardAlert #$kAlertNoteAlert msg
                     (ccl:%null-ptr) (ccl:%null-ptr) (ccl:%null-ptr)))
  (#_RunApplicationEventLoop)
  (ccl:quit))

(defun cm-image-dir ()
  (namestring
   (make-pathname
    :directory (pathname-directory ccl::*heap-image-name*))))

(defun save-cm (path &rest args)
  (declare (ignore args) (special *cm-readtable*))
  (setf ccl::*inhibit-greeting* t)
  (setf ccl:*lisp-startup-functions*
        (append ccl:*lisp-startup-functions*
                (list #'(lambda ()
                          (declare (special *cm-readtable*))
                          (setf *package* (find-package :cm))
                          (setf *readtable* *cm-readtable*)
                          (load-cminit)
                          (cm-logo)
                          ))))
  (ccl:save-application path :application-class *cm-application-class*))


#||
(defun save-cm (&optional path &rest args &key carbon slime-directory 
			  swank-port)
  ;; if user calls this function, path is path directory to save app in.
  ;; else (called by make-cm) path is  cm/bin/openmcl*/cm.image
  (declare (ignore args) (special *cm-readtable*))
  (let* ((cmroot (symbol-value 'cl-user::*cm-directory*))
         (appdir (cond ((and (not path)
                             (boundp 'cl-user::binary-dir))
                        (symbol-value 'cl-user::binary-dir))
                       ((ccl:directoryp path)
                        (probe-file path))
                       ((equal (pathname-type path) "image")
                        (make-pathname :name nil :type nil
                                       :defaults path))
                       (t
                        (error "save-cm: ~s is not a directory." 
                               path))))
         (bundle (merge-pathnames "CM.app/" appdir))
         (resdir (merge-pathnames "Contents/Resources/" bundle))
         (etcdir (merge-pathnames "Contents/Resources/etc/" bundle))
         (libdir (merge-pathnames "Contents/Resources/lib/" bundle))
         (exedir (merge-pathnames "Contents/MacOS/" bundle)))
    (unless (probe-file bundle)
      (ccl:create-directory bundle)
      (ccl:create-directory resdir)
      (ccl:create-directory exedir)
      (ccl:create-directory libdir)
      (ccl:create-directory etcdir))
    ;; CM.app/info.plist
    (create-info.plist (merge-pathnames "Contents/Info.plist" bundle)
                       carbon)
    ;; cm/etc/xemacs -> Contents/Resources/etc/*.el
    (ccl:copy-file (merge-pathnames "etc/xemacs/listener.el" cmroot)
                   (merge-pathnames "listener.el" etcdir)
                   :if-exists :supersede)
    (ccl:copy-file (merge-pathnames "etc/xemacs/cm.el" cmroot)
                   (merge-pathnames "cm.el" etcdir)
                   :if-exists :supersede)
    ;; dppccl->MacOS/cm
    (ccl:copy-file (car ccl::*command-line-argument-list*)
                   (merge-pathnames "cm" exedir)
                   :if-exists :supersede)
    (create-cm.sh (merge-pathnames "cm.sh" exedir))
    (if carbon
      (carbon-setup slime-directory swank-port)
      (normal-setup))
    (ccl:save-application (merge-pathnames "cm.image" exedir)
                          :application-class *cm-application-class*)))
||#

(defun normal-setup ()
  (setf ccl::*inhibit-greeting* t)
    (setf ccl:*lisp-startup-functions*
        (append ccl:*lisp-startup-functions*
                (list #'(lambda ()
                          (declare (special *cm-readtable*))
                          (setf *package* (find-package :cm))
                          (setf *readtable* *cm-readtable*)
                          (load-cminit)
                          (cm-logo) )))))

(defun carbon-setup (slime-directory swank-port)
  (setf *cm-application-class* (find-class 'cm-carbon-application))
  (when slime-directory
    (if (ccl:directoryp slime-directory) ; ensure / at end
        (setf slime-directory (probe-file slime-directory))
	(error "save-cm: :slime-directory ~s is not a directory."
	       slime-directory))
    (let ((files '("swank-backend" "nregex" "metering" "swank-openmcl" "swank-gray"
                   "swank")))
      (dolist (f files)
        (load (merge-pathnames f slime-directory)))))
  (when swank-port
    (unless (find-package :swank)
      (error "save-cm: :swank-port specified but swank is not loaded. Specify :slime-directory to load it."))
    (setf *cm-swank-port* swank-port)))

(defun create-info.plist (path carbon?)
  (with-open-file (fil path :direction :output :if-does-not-exist :create
                       :if-exists :supersede)
    (format fil
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>English</string>
	<key>CFBundleExecutable</key>
	<string>~A</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>CFBundleVersion</key>
	<string>0.2</string>
	<key>NSMainNibFile</key>
	<string>MainMenu</string>
</dict>
</plist>
" (if carbon? "cm" "cm.sh"))))

(defun create-cm.sh (path)
  (with-open-file (fil path :direction :output :if-does-not-exist :create
                       :if-exists :supersede)
    (format fil
"#!/bin/sh 
CWD=\"`(cd \\\"\\`dirname \\\\\\\"$0\\\\\\\"\\`\\\"; echo $PWD)`\"
RES=\"`dirname \\\"$CWD\\\"`/Resources\"
EDITOR=`defaults read -app CM editor 2>/dev/null`
export \"DISPLAY=:0.0\"
if [[ ! $EDITOR || $EDITOR == Emacs ]] ; then
    EDITOR=\"/Applications/Emacs.app/Contents/MacOS/Emacs\"
fi

if [ -f \"$EDITOR\" ] ; then
    \"$EDITOR\" -l \"${RES}/etc/listener.el\" -l \"${RES}/etc/cm.el\" --eval \"(lisp-listener \\\"${CWD}/cm --image-name ${CWD}/cm.image\\\" )\"
else
    open -a Terminal \"${CWD}/cm\"
fi

#EOF
"))
  (shell (format nil "chmod a+x ~A" (namestring path)))
  )

;;;
;;; midishare callbacks moved here.
;;;

;; (ccl:defcallback run-proc (:unsigned-fullword date :unsigned-halfword refnum
;;                                               :unsigned-fullword indx
;;                                               :unsigned-fullword arg1
;;                                               :unsigned-fullword arg2)
;;   (declare (ignore arg1 arg2)
;;            (function rem-proc)
;;            (special *qstart* *qtime* *qnext* *proctable*))
;;   (setf *qstart* 0)                     ; unused here
;;   (setf *qtime* date)                   ; current time
;;   (setf *qnext* date)                   ; 'wait' sets this ahead.
;;   ;; funcall the process fn until its next run time is
;;   ;; in the future or the process is dead (returned nil)
;;   (do ((proc (elt *proctable* indx))
;;        (alive t))
;;       ((or (not alive)                 ; stop if process killed itself
;;            (> *qnext* *qtime*))        ; or need to reschedule
;;        (if alive
;;          (ms:MidiTask run-proc *qnext* refnum indx 0 0)
;;          (rem-proc indx))
;;        (values))
;;     (setq alive (funcall proc))))

;; (ccl:defcallback midi-receive-hook (:unsigned-halfword refnum)
;;   (declare (special *receive-hook* *mp*))
;;   (restart-case
;;       (handler-bind ((error
;;                       #'(lambda (c)
;;                           (declare (ignore c))
;;                           (invoke-restart 'callback-error-exit))))
;;         ;;; the receive loop...
;;         (do ((go t)
;;              (ev (ms:MidiGetEv refnum) (ms:MidiGetEv refnum)))
;;             ((or (not go) (ms:nullptrp ev))
;;              (values))
;;           (if *receive-hook*
;;             (funcall *receive-hook* ev)
;;             (setf go nil))))
;;     (callback-error-exit () 
;;       (format t "~&Caught error under MIDI callback! Exiting receive.~&")
;;       ;;(ms:MidiFreeEv e)
;;       (ms:MidiFlushEvs *mp*)
;;       (setf *receive-hook* nil)
;;       (ms:MidiSetRcvAlarm *mp* (ms:nullptr))
;;       (values))))

;;;

