;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2003, Oliver Markovic <entrox@entrox.org> 
;;;   All rights reserved. 
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  o Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer. 
;;;  o Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution. 
;;;  o Neither the name of the author nor the names of the contributors may be
;;;    used to endorse or promote products derived from this software without
;;;    specific prior written permission. 
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :org.entrox.package-aliases)


(defvar *package-alias-table* (make-hash-table))


(define-condition package-alias-error (error)
  ((alias-name :initarg :alias-name :initform nil
               :reader package-alias-error.alias-name)
   (alias-package-name :initarg :alias-package-name :initform nil
                       :reader package-alias-error.alias-package-name)))


(define-condition conflict-with-package (package-alias-error package-error)
  ((conflicting-nickname :initarg :conflicting-nickname :initform nil
                         :reader conflicting-nickname))
  (:report (lambda (c s)
             (format s "Alias ~A in package ~A conflicts with "
                     (package-alias-error.alias-name c)
                     (package-alias-error.alias-package-name c))
             (if (conflicting-nickname c)
                 (format s "nickname ~A of package ~A."
                         (conflicting-nickname c)
                         (package-name (package-error-package c)))
                 (format s "package ~A."
                         (package-name (package-error-package c)))))))


(define-condition conflict-with-alias (package-alias-error)
  ((nickname-p :initarg :nickname-p :initform nil
               :reader conflict-with-alias.nickname-p))
  (:report (lambda (c s)
             (format s "~:[Nickname~;Packagename~] ~A conflicts with alias in package ~A."
                     (conflict-with-alias.nickname-p c)
                     (package-alias-error.alias-name c)
                     (package-alias-error.alias-package-name c)))))

(define-condition package-still-aliased (package-alias-error package-error)
  ((aliasing-packages :initarg :aliasing-packages :initform nil
                      :reader package-still-aliased.aliasing-packages))
  (:report (lambda (c s)
             (format s "Package ~A is still being aliased by ~A."
                     (package-name (package-error-package c))
                     (package-still-aliased.aliasing-packages c)))))


;;
;; The package alias structure holds a source package in FROM and
;; a string in TO, which designates the alias name.
;;

(defstruct (package-alias
             (:constructor %make-package-alias)
             (:predicate package-alias-p)
             (:print-object package-alias-printer))
  from
  to)


(defun make-package-alias (package source-package alias-name)
  "Make a new package alias in package PACKAGE, which translates the name in
ALIAS-NAME to the package given in SOURCE-PACKAGE."
  (check-type package (not null))
  (check-type source-package (not null))
  (check-type alias-name (not null))

  (let* ((package-object (ensure-package source-package))
         (alias-name-string (string alias-name))
         (alias-list (package-alias-list package))
         (new-alias (%make-package-alias :from package-object
                                         :to alias-name-string)))

    ;; check for collisions
    (dolist (p (list-all-packages))
      (when (string-equal (package-name p) alias-name-string)
        (error 'conflict-with-package
               :alias-name alias-name-string
               :alias-package-name (package-name package-object)
               :package p))
      (dolist (nick (package-nicknames p))
        (when (string-equal nick alias-name-string)
          (error 'conflict-with-package
                 :alias-name alias-name-string
                 :alias-package-name (package-name package-object)
                 :package p
                 :conflicting-nickname nick))))
    ;; check for duplicates
    (unless (dolist (alias alias-list)
              (when (package-alias-equal alias new-alias)
                (return t)))
      (setf (%package-alias-list package) (pushnew new-alias alias-list)))
    new-alias))

(defun package-alias-equal (alias1 alias2)
  "Check for equality between two package aliases. Does NOT check the package,
in which it is active."
  (check-type alias1 package-alias)
  (check-type alias2 package-alias)
  (if (and (eql (package-alias-from alias1) (package-alias-from alias2))
           (string-equal (package-alias-to alias1) (package-alias-to alias2)))
      t
      nil))

(defun package-alias-printer (object stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~A\" -> \"~A\"" (package-name (package-alias-from object))
            (package-alias-to object))))
                                    
;;;
;;; Define new operators on packages
;;;

;; internal
(defun ensure-package (package)
  (let ((package-object (original-find-package package)))
    (loop
       (when package-object
         (return package-object))
       (cerror "Enter another package name."
               "No such package ~A." package)
       (setf package-object (original-find-package (read))))))

(defun package-alias-list (package)
  "Returns a list of aliases in the given package."
  (check-type package (not null))
  (values (gethash (ensure-package package) *package-alias-table*)))

;; internal
(defun (setf %package-alias-list) (value package)
  (setf (gethash (ensure-package package) *package-alias-table*) value))

(defun package-aliased-by-list (package)
  "Returns a list of packages, which have an alias on the given package."
  (check-type package (not null))
  (let ((package-object (ensure-package package))
        (aliased-by-list nil))
    (with-hash-table-iterator (next-entry *package-alias-table*)
      (loop (multiple-value-bind (more? key value) (next-entry)
              (unless more?
                (return aliased-by-list))
              (dolist (alias value)
                (when (eql (package-alias-from alias) package-object)
                  (pushnew key aliased-by-list))))))))

(defun map-over-package-aliases (function package)
  "Applies FUNCTION to all aliases in PACKAGE."
  (check-type function (not null))
  (check-type package (not null))
  (dolist (alias (package-alias-list package))
    (funcall function alias)))

(defun delete-package-alias (package alias)
  "Removes ALIAS from the list of package aliases in PACKAGE."
  (check-type package (not null))
  (check-type alias (not null))
  (let* ((alias-list (package-alias-list package)))
    (setf (%package-alias-list package)
          (remove-if (lambda (x) (package-alias-equal x alias)) alias-list))))

(defun delete-all-aliases-in-package (package)
  "Removes all package aliases in PACKAGE"
  (check-type package (not null))
  (remhash package *package-alias-table*))

(defun list-all-package-aliases ()
  "Returns a list of all known package aliases in the form (PACKAGE . ALIAS)."
  (let ((alias-list nil))
    (with-hash-table-iterator (next-entry *package-alias-table*)
      (loop (multiple-value-bind (more? key value) (next-entry)
              (unless more?
                (return alias-list))
              (dolist (alias value)
                (pushnew (cons key alias) alias-list)))))))

;; only used by the reader
(defun %get-aliases-for-package (current-package source-package)
  (declare (optimize (speed 3) (safety 0)))
  (dolist (alias (gethash current-package *package-alias-table*))
    (when (equal (package-alias-from alias) source-package)
      (return (package-alias-to alias)))))

;; used by DEFPACKAGE, MAKE-PACKAGE and RENAME-PACKAGE
(defun %check-package-for-conflicts (name nicknames)
  (let ((name-string (string name))
        (nicknames-string-list (mapcar #'string nicknames)))
    (dolist (alias-entry (package-aliases:list-all-package-aliases))
      ;; check the package name
      (when (string-equal (package-aliases:package-alias-to (cdr alias-entry))
                          name-string)
        (error 'conflict-with-alias
               :alias-name name-string
               :alias-package-name (package-name (car alias-entry))))
      ;; check all nicknames
      (dolist (nick nicknames-string-list)
        (when (string-equal (package-aliases:package-alias-to (cdr alias-entry))
                            nick)
          (error 'conflict-with-alias
                 :alias-name name-string
                 :alias-package-name (package-name (car alias-entry))
                 :nickname-p t))))))

;; used by DEFPACKAGE
;; this is tacky - the alias-list isn't full of alias objects, but of conses,
;; because we have to check for conflicts /before/ they get added
(defun %check-aliases-for-conflicts (alias-list package-name)
  (dolist (alias alias-list)
    (when (find-package (cdr alias))
      (error 'conflict-with-package
             :alias-name (cdr alias)
             :alias-package-name package-name
             :package (find-package (cdr alias))))))
    

;; used by RENAME-PACKAGE
(defun %redirect-aliases-on-package-rename (old-package new-package)
  (when (and old-package new-package)
    (dolist (aliasing-package (package-aliased-by-list old-package))
      (dolist (alias (package-alias-list aliasing-package))
        (when (eql (package-alias-from alias) old-package)
          (setf (package-alias-from alias) new-package))))))

;; used by DELETE-PACKAGE
(defun %maybe-purge-package-aliases (package-object)
  (when package-object
    (let ((aliased-by-list (package-aliased-by-list package-object)))
      (when aliased-by-list
        (restart-case
            (error 'package-still-aliased
                   :package package-object
                   :aliasing-packages aliased-by-list)
          (remove-aliases ()
            :report "Remove the aliases."
            (dolist (aliasing-package aliased-by-list)
              (dolist (alias (package-alias-list aliasing-package))
                (when (eql (package-alias-from alias) package-object)
                  (delete-package-alias aliasing-package alias))))))))))


;;; arch-tag: F192FCF8-2E69-11D8-A714-000393D3F814
