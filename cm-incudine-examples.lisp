(ql:quickload "cm-incudine")
;;; enter common music:

(cm)

;;; start incudine's realtime scheduler

(rts)

;;; open jack midi output

(midi-open-default :direction :output)

;;; after connecting a midi device via qjackctl

(sprout (new midi :time 0))

;;; fudi:
;;; open pd, create an object "netreceive 3001" and connect a "print" object to its left outlet

;;; open the connection fomr cm to pd:

(fudi-open-default :direction :output)

;;; send a message:

(output (new fudi :time 0 :message "Hallo Pd"))
