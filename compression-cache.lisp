(in-package :cl-user)

(defpackage compression-cache
  (:use :cl)
  (:import-from :clache)
  (:import-from :salza2))

(in-package :compression-cache)

(defvar *store-path* NIL)

(defun initialize-cache (cache-path)
  ;; check that cache-path is either a string or a pathname
  ;; if it i's a string, turn into a pathname
  ;; then make sure the pathname points to an valid directory
  ;; if the pathname does not point to a valid directory
  ;; signal the condition directory doesn't exist and display the directory
  ;; if the input was not either of those types, signal a condition
  ;; invalid input type or something (check-type)
  ;; This avoid running into issues later on, easier to catch early.
  (uiop:ensure-pathname cache-path
                        :want-pathname T
                        :ensure-directory T
                        :ensure-directories-exist T)
  (setf *store-path* cache-path))

(defvar *cache-store*
  (progn
    (ensure-directories-exist #p"cache/")
    (make-instance 'file-store :directory #p"cache/")))

;; Store cache
(setcache 1 "foo" *store*)
;;=> 1

;; Get cache
(getcache 1 *store*)
;;=> 1, T

;; Get non-exited cache
(getcache 42 *store*)
;;=> NIL, NIL

;; Remove cache
(remcache 1 *store*)
;;=> T

;; Clear all cache
(clrcache *store*)
