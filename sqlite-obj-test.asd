(asdf:defsystem sqlite-obj-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "Tests for a compact, somewhat incorrect ORM for sqlite. Really, don't use this."
  :depends-on (#:fiveam #:sqlite-obj)
  :components
  ((:module test
    :serial t
    :components
    ((:file "main")))))

