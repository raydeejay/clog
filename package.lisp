;;;; package.lisp

(defpackage #:clog
  (:use #:cl
        #:cl-who
        #:parenscript
        #:lass
        #:split-sequence
        #:mustache
        #:3bmd
        #:slug)
  (:export #:export-blog
           #:process-posts)
  (:import-from #:alexandria
                #:assoc-value
                #:plist-alist
                #:read-file-into-string))

