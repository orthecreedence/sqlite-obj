(asdf:defsystem sqlite-obj
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.2"
  :description "A compact, somewhat incorrect ORM for sqlite. Really, don't use this."
  :depends-on (#:sqlite #:vom #:yason)
  :components
  ((:file "package")
   (:file "database" :depends-on ("package"))
   (:file "operations" :depends-on ("package" "database"))
   (:file "schema" :depends-on ("package" "database" "operations"))))

