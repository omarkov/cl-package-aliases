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


;; The following is taken from McCLIM:
;; FIXME: I'd rather not disturb *features*
(eval-when (:compile-toplevel :execute)
  (when (find-symbol "PACKAGE-LOCK" :ext)
    (pushnew 'package-aliases::package-locks *features*)))

#+package-aliases::package-locks
(eval-when (:load-toplevel)
  (unless (find-symbol "PACKAGE-LOCK" :ext)
    (error "Binary incompatibility: your CMUCL does not have package locks")))

(defmacro with-kernel-redefinition (&body body)
  #+package-aliases::package-locks
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) t)))
  #-package-aliases::package-locks
  `(progn ,@body))


;;;
;;; Redefine CL:MAKE-PACKAGE to check for conflicts with aliases
;;;
(defun patched-make-package (name &key
                             (use ext:*default-package-use-list*)
                             nicknames
                             (internal-symbols 10)
                             (external-symbols 10))
  (package-aliases::%check-package-for-conflicts name nicknames)
  (original-make-package name :use use :nicknames nicknames
                         :internal-symbols internal-symbols
                         :external-symbols external-symbols))

;;;
;;; Redefine CL:FIND-PACKAGE to also look for aliases
;;;
(defun patched-find-package (package)
  (let ((package-object (package-aliases::original-find-package package)))
    (unless package-object
      ;; no normal package was found, try the aliases
      (let ((package-string (string package)))
        (dolist (alias (package-alias-list *package*))
          (when (string-equal package-string
                              (package-aliases:package-alias-to alias))
            (setf package-object (package-aliases:package-alias-from alias))
            (return)))))
    package-object))

;;;
;;; Apply the patches
;;;
(with-kernel-redefinition
    (eval-when (:load-toplevel :execute)
      ;; MAKE-PACKAGE
      (setf (fdefinition 'cl:make-package)
            (fdefinition 'package-aliases::patched-make-package))
      ;; FIND-PACKAGE
      (setf (fdefinition 'cl:find-package)
            (fdefinition 'package-aliases::patched-find-package))
      ;; RENAME-PACKAGE
      (setf (fdefinition 'cl:rename-package)
            (fdefinition 'package-aliases::patched-rename-package))
      ;; DELETE-PACKAGE
      (setf (fdefinition 'cl:delete-package)
            (fdefinition 'package-aliases::patched-delete-package))
      ;; DEFPACKAGE
      (setf (macro-function 'cl:defpackage) 
            (macro-function 'package-aliases::patched-defpackage))))


;;; arch-tag: 20D863A8-2E6A-11D8-8C0C-000393D3F814
