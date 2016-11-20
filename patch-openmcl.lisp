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

(defmacro with-kernel-redefinition (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf ccl::*warn-if-redefine-kernel* nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf ccl::*warn-if-redefine-kernel* t))))

;; save the definition of %find-pkg
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'original-%find-pkg)
    (setf (fdefinition 'original-%find-pkg)
          (fdefinition 'ccl::%find-pkg))))

;;;
;;; Redefine CL:MAKE-PACKAGE to check for conflicts with aliases
;;;
(defun patched-make-package (name &key
                             nicknames
                             (use ccl::*make-package-use-defaults*)
                             (internal-size 60)
                             (external-size 10))
  (package-aliases::%check-package-for-conflicts name nicknames)
  (original-make-package name :use use :nicknames nicknames
                         :internal-size internal-size
                         :external-size external-size))

(in-package :ccl)
(package-aliases::with-kernel-redefinition
    ;;;
    ;;; Patch the package finding system to include aliases
    ;;;
    (defun patched-%find-pkg (name &optional (len (length name)))
      (declare (fixnum len) (optimize (speed 3) (safety 0)))
      (with-package-list-read-lock
          ;; search all packages
          (dolist (p %all-packages%)
            (let ((package-names (pkg.names p))
                  (package-aliases (package-aliases::%get-aliases-for-package *package* p)))
              ;; append the possible aliases for this package
              (when package-aliases
                (pushnew package-aliases package-names))
              ;; go through the name, nicknames and aliases
              (if (dolist (pkgname package-names)
                    (when (and (= (length pkgname) len)
                               (dotimes (i len t)
                                 ;; Aref: allow non-simple strings
                                 (unless (eq (aref name i) (schar pkgname i))
                                   (return))))
                      (return t)))
                  (return p)))))))

;;;
;;; Apply the patches
;;;
(package-aliases::with-kernel-redefinition
    (eval-when (:load-toplevel :execute)
      ;; %FIND-PKG
      (setf (fdefinition 'ccl::%find-pkg)
            (fdefinition 'ccl::patched-%find-pkg))
      ;; MAKE-PACKAGE
      (setf (fdefinition 'cl:make-package)
            (fdefinition 'package-aliases::patched-make-package))
      ;; RENAME-PACKAGE
      (setf (fdefinition 'cl:rename-package)
            (fdefinition 'package-aliases::patched-rename-package))
      ;; DELETE-PACKAGE
      (setf (fdefinition 'cl:delete-package)
            (fdefinition 'package-aliases::patched-delete-package))
      ;; DEFPACKAGE
      (setf (macro-function 'cl:defpackage)
            (macro-function 'package-aliases::patched-defpackage))))


;;; arch-tag: 4C3B7BF5-2E6A-11D8-8B88-000393D3F814
