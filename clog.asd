;;;; clog.asd

(asdf:defsystem #:clog
  :description "A simple static blog generator"
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:split-sequence
               #:alexandria
               #:cl-who
               #:parenscript
               #:lass
               #:3bmd
               #:3bmd-ext-code-blocks
               #:3bmd-ext-wiki-links
               #:3bmd-ext-definition-lists
               #:3bmd-ext-tables
               #:cl-mustache
               #:cl-slug)
  :components ((:file "package")
               (:file "clog")))
