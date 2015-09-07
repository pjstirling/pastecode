(asdf:defsystem #:pastecode
  :serial t
  :depends-on (#:pjs-utils #:pjs-webapp)
  :components ((:file "package")
               (:file "pastecode")))
