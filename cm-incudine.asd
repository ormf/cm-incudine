;;;; cm-incudine.asd
;;;;
;;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem "cm-incudine"
  :version "0.0.1"
  :description "Extention of commmon music for realtime i/o using incudine."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later."
  :depends-on (#:incudine
               #:cm
               #:fudi)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "incudine")
                             (:file "incudine-rts")
                             (:file "rt")
                             (:file "osc")
                             (:file "fudi")
                             (:file "jackmidi")
                             (:file "io")
                             (:file "exports")))))
