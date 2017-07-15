;;;; cm-incudine.asd
;;;;
;;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:cm-incudine
  :description "Extention of commmon music for realtime io using incudine."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later."
  :depends-on (#:incudine
               #:cm
               #:fudi)
  :serial t
  :components ((:file "package")
               (:file "cm-incudine")
               (:file "incudine")
               (:file "incudine-rts")
               (:file "rt")
               (:file "osc")
               (:file "fudi")
               (:file "jackmidi")
               ))

