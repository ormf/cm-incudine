(in-package :cl-user)

(load 
 (make-pathname :name "cm" :type "asd"
                :directory (butlast (pathname-directory *load-pathname*))
                :defaults *load-pathname*))

(use-system :cm)

;;; add other use-systems here to autoload them at startup. see
;;; cm/doc/install.html for more information.

; (use-system :fomus)

