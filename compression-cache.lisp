(in-package :cl-user)

(defpackage compression-cache
  (:use :cl)
  (:import-from :uiop)
  (:import-from :clache)
  (:import-from :salza2)
  (:export 
   :initialize-cache
   :ensure-path-to-compressed-file))

(in-package :compression-cache)

(defvar *store-path* NIL)
(defvar *root* (uiop:getcwd))
(defvar *store* NIL)

(defun initialize-cache (cache-path &optional root)
  "cache-path can be either a pathname or a string of a path to a directory. If the directory doesn't exist it will be created. If a relative directory is provided AKA one not starting with a forward slash '/', the pathname will be derived from the current working directory `(uiop:getcwd)`"
  ;; check that cache-path is either a string or a pathname
  ;; if it is a string, turn it into a pathname
  ;; then make sure the pathname points to an valid directory
  ;; if the pathname does not point to a valid directory
  ;; signal the condition directory is not valid
  ;; if the input was not either of those types, signal a condition
  ;; This avoid running into issues later on, easier to catch early.
  (let ((valid-cache-pathname (uiop:ensure-pathname cache-path
                                                    :want-pathname T
                                                    :ensure-directory T
                                                    :ensure-directories-exist T)))
    (setf *store-path* valid-cache-pathname)
    (setf *root* root)
    (setf *store*
          (make-instance 'clache:file-store :directory valid-cache-pathname))))

(defun ensure-path-to-compressed-file (filepath &optional &key (algorithm :gzip))
  ;; TODO add check-if-modified check. Need to add the date to the value of the cache (not the key)
  ;; and then check the modification time of the original file vs the saved value in the cache
  ;; see https://lispcookbook.github.io/cl-cookbook/files.html#getting-file-attributes-size-access-time
  ;; https://osicat.common-lisp.dev/
  ;; https://github.com/Shinmera/file-attributes/
  ;; https://shinmera.github.io/file-attributes/
  ;; (check-if-modified NIL)
  "Will check if there is a path for a compressed version of the original pathname and return it, if there is none, it will then compress a file and save it and return the compressed file's path."
  (assert (uiop:file-exists-p (uiop:merge-pathnames* filepath *root*))
          (filepath) "File ~A not found" filepath)
  (let* ((valid-pathname (uiop:ensure-pathname filepath
                                               :want-pathname T))
         (key (get-cache-key valid-pathname :algorithm algorithm))
         (cache-value (clache:getcache key *store*)))
    (if cache-value cache-value
        (compress-and-cache valid-pathname :algorithm algorithm))))

(defun get-cache-key (filepath &optional &key (algorithm :gzip))
  (clache:cache-key-to-string
   (list :algorithm algorithm :path filepath)))

(defun compress-file (filepath &optional &key (algorithm :gzip))
  (assert (equal :gzip algorithm)
          (algorithm)
          "Compression Algorithm ~A is not available. Currently only `:gzip` is supported"
          algorithm)
  (let* ((merged-pathname (uiop:merge-pathnames* filepath *store-path*))
         (compressed-filepath (uiop:ensure-pathname merged-pathname
                                                    :want-pathname T
                                                    :ensure-directories-exist T))
         (static-filepath (uiop:merge-pathnames* filepath *root*)))
    ;; (with-open-file (istream filepath :element-type '(unsigned-byte 8))
    ;;   (with-open-file (ostream compressed-filepath
    ;;                            :element-type '(unsigned-byte 8)
    ;;                            :direction :output
    ;;                            :if-exists :supersede
    ;;                            :if-does-not-exist :create)
    ;;     (salza2:gzip-stream istream ostream)))
    ;; (probe-file compressed-filepath)
    (salza2:gzip-file static-filepath compressed-filepath)
    compressed-filepath))

(defun save-to-cache (original-filepath compressed-filepath &optional &key (algorithm :gzip))
  (clache:setcache (get-cache-key original-filepath :algorithm algorithm)
                   compressed-filepath
                   *store*))

(defun compress-and-cache (filepath &optional &key (algorithm :gzip))
  (let ((compressed-filepath (compress-file filepath :algorithm algorithm)))
    (save-to-cache filepath compressed-filepath :algorithm algorithm)
    compressed-filepath))

