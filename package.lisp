;;;; package.lisp

(defpackage #:clog
  (:use #:cl
        #:cl-who
        #:parenscript
        #:lass
        #:split-sequence
        #:mustache
        #:3bmd
        #:slug
        #:cl-arrows)
  (:export #:export-blog
           #:process-posts)
  (:import-from #:alexandria
                #:assoc-value
                #:plist-alist
                #:read-file-into-string
                #:write-string-into-file))

