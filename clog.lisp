;;;; clog.lisp

(in-package #:clog)

;;; "clog" goes here. Hacks and glory await!

;;; config
(defparameter *default-config* `((:source . "/path/to/blog")
                                 (:output . "/path/for/output")
                                 (:remote . "remote.host.com")
                                 (:url . "http://your.blog.url")))
(defparameter *config* nil)

(defun create-config (path)
  (with-open-file (config-file path :direction :output :if-exists :supersede)
    (print *default-config* config-file)
    (terpri config-file)))

(defun read-config (path)
  (when (not (probe-file path))
    (create-config path))
  (with-open-file (f path :direction :input)
    (read f)))

(defun make-a-path (base node)
  (format nil "~A/~A" base node))

(defun make-config-path (key node)
  (make-a-path (assoc-value *config* key) node))

;;; skeleton
;; provide function to write skeleton to disk, MAKE-BLOG or so
;; write lisp functions using cl-who, lass and perhaps parenscript, to generate that skeleton
;; write config tailored to that skeleton too
(defun navbar ()
  (with-html-output-to-string (s nil :indent t)
    (:div :class "navigation"
          (:a :href (make-config-path :url "") "HOME")
          (:a :href (make-config-path :url "posts/") "INDEX")
          (:a :href (make-config-path :url "tags/") "TAGS")
          (:a :href (make-config-path :url "archive/") "ARCHIVE"))))

(defun footer ()
  (with-html-output-to-string (s nil :indent t)
    (:div :class "footer fineprint" "(C) 2016 Sergi Reyner. All rights reserved (for now)." (:span :class "powered" "Powered by " (:a :href "https://github.com/raydeejay/clog" "Clog") "."))))

(defun add-boilerplate (content)
  "Fill a page template with the content"
  (with-html-output-to-string (s nil :prologue T :indent T)
    (:html
      (:head
       (:link :rel "stylesheet" :href (make-config-path :url "css/style.css")) ; use LASS
       (:link :rel "stylesheet" :href (make-config-path :url "css/colorize.css"))
       (:link :rel "stylesheet" :href (make-config-path :url "css/highlight.css")) ; use LASS
       (:script :src (make-config-path :url "js/highlight.pack.js"))
       (:script "hljs.initHighlightingOnLoad();")) ; use Parenscript
      (:body (str (navbar))
             (str content)
             (str (footer))))))

;; (defun make-directories (path)
;;   (mapc (lambda (subdir)
;;           (ensure-directories-exist (concatenate 'string path "/" subdir)
;;                                     :mode 755))
;;         '("templates" "styles" "scripts"))
;;   (ensure-directories-exist (concatenate 'string path "/scripts") :mode 755)
;;   (ensure-directories-exist (concatenate 'string path "/styles") :mode 755))

;; (defun make-blog-skeleton (path)
;;   "Create a skeleton, with the default files in the proper places."
;;   (make-directories path)
;;   (make-templates path)
;;   (make-styles path)
;;   (make-scripts path))

;;; exporting
(defun export-content (content path)
  "Write a string to a file."
  (with-open-file (f path :direction :output :if-exists :supersede)
    (format f "~A~%" content)))

(defmethod 3bmd-wiki:process-wiki-link ((wiki T) normalized-target formatted-target args stream)
  (format stream "<a href='~A'>~A</a>"
          formatted-target
          (or (car args) formatted-target)))

(defun expand-markdown (md)
  (with-output-to-string (s)
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd-wiki:*wiki-links* t)
          (3bmd-wiki:*wiki-processor* t)
          (3bmd-definition-lists:*definition-lists* t)
          (3bmd-tables:*tables* t))
      (3bmd:parse-string-and-print-to-stream md s))))

(defun expand-mustache (template data)
  (mustache:render* template data))

;;; posts
(defun load-post-metadata (stream)
  (loop :for line := (read-line stream)
        :for header := (car (split-sequence #\Space line :count 1))
        :for value := (string-trim " " (subseq line (length header)))
        :when (string-equal header "title:")
        :collect (cons :title value)
        :when (string-equal header "author:")
        :collect (cons :author value)
        :when (string-equal header "date:")
        :collect (cons :date value)
        :when (string-equal header "tags:")
        :collect (cons :tags (split-sequence #\Space value :remove-empty-subseqs t))
     :until (string-equal line "")))

(defun attach-extra-metadata (post)
  (append post (plist-alist (list :slug (slug:slugify (assoc-value post :title))
                                  :next nil
                                  :prev nil))))

(defun attach-content (post content)
  (append post (plist-alist (list :content content))))

(defun load-post (path)
  "Takes a path to a Markdown file and returns a string of HTML."
  (with-open-file (stream path)
    (let ((filesize (file-length stream))
          (metadata (attach-extra-metadata (load-post-metadata stream))))
      (let* ((content-size (- filesize (file-position stream)))
             (content (make-string content-size)))
        (read-sequence content stream)
        (attach-content metadata content)))))

;;; list of posts
(defun load-directory (path extension)
  (loop :for f :in (directory (concatenate 'string path "/*" extension))
     :collecting (load-post f)))

(defun sort-posts! (posts)
  (sort posts #'string-greaterp :key (lambda (post) (assoc-value post :date))))

(defun sort-tags! (tags)
  (sort tags #'string-greaterp :key (lambda (tag) (assoc-value tag :date))))

(defun link-posts! (prev next)
  (rplacd (assoc :prev next)
          (list (cons :link (assoc-value prev :slug))))
  (rplacd (assoc :next prev)
          (list (cons :link (assoc-value next :slug)))))

(defun expand-post! (post)
  (rplacd (assoc :content post)
          (expand-markdown (assoc-value post :content))))

(defun link-and-expand! (posts)
  (loop :for prev := nil :then post
     :for post :in posts
     :doing (progn (when prev (link-posts! prev post))
                   (expand-post! post))
     :finally (return posts)))

(defun export-pipeline (template data output-pathname)
  (-> (make-config-path :source template)
      (read-file-into-string)
      (expand-mustache data)
      (add-boilerplate)
      (export-content output-pathname)))

(defun process-post-index (posts)
  (export-pipeline "posts-index.mustache"
                   (plist-alist (list :elements posts))
                   (make-config-path :output "posts/index.html")))

(defun process-tag-index (tags)
  (loop :for tag :being :the :hash-keys :in tags :using (hash-value slugs)
     :do (export-pipeline "tag.mustache"
                          (plist-alist (list :tag tag :elements slugs))
                          (make-config-path :output (format nil "tags/~A" tag)))
     :collecting tag :into tags-list
     :finally (export-pipeline "tags-index.mustache"
                               (plist-alist (list :elements tags-list))
                               (make-config-path :output "tags/index.html"))))

(defun process-posts (path output-path)
  "Takes a directory path string, and iterates over all the .post
files in it, generating a corresponding HTML file in the output
directory, without any extension. This gets us nice URLs."
  (let ((posts (link-and-expand! (sort-posts! (load-directory path ".post")))))
    (loop :for post :in posts
       :for link-file := (make-config-path :output "index.html") :then NIL
       :with tags := (make-hash-table)
       :doing (let ((output-pathname (make-pathname :name (assoc-value post :slug)
                                                    :type :unspecific
                                                    :directory output-path)))
                (mapc (lambda (tag)
                        (push `((:slug . ,(assoc-value post :slug)))
                               (gethash tag tags (list))))
                      (assoc-value post :tags))
                (export-pipeline "template.mustache" post output-pathname)
                (when link-file
                  (when (probe-file link-file) (sb-posix:unlink link-file))
                  (sb-posix:symlink (namestring output-pathname) link-file)))
       :finally (progn (process-post-index posts)
                       (process-tag-index tags)))))

(defun copy-file-to-directory (pathspec output-pathspec)
  (let* ((from (pathname pathspec))
         (to (merge-pathnames (make-pathname :directory output-pathspec) from)))
    (alexandria:copy-file from to)))

(defun process-additional-files ()
  (mapc (lambda (cell)
          (copy-file-to-directory (make-config-path :source (car cell))
                                  (make-config-path :output (cdr cell))))
        `(("css/style.css" . "css")
          ("css/colorize.css" . "css")
          ("css/highlight.css" . "css")
          ("js/highlight.pack.js" . "js"))))

;;; entry points
(defun export-blog ()
  (let ((*config* (read-config (merge-pathnames (make-pathname :name ".clogrc")
                                                (user-homedir-pathname)))))
    (make-output-directories)
    (process-posts (assoc-value *config* :source)
                   (assoc-value *config* :output))
    (process-additional-files)))

(defun make-output-directories ()
  (mapc (lambda (dir)
          (ensure-directories-exist (make-config-path :output dir) :mode 511))
        '("css/" "files/" "images/" "js/" "posts/" "tags/")))
