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


;;;
;;; Patch DEFPACKAGE to include the :ALIAS option, so we can define package
;;; aliases immediately.
;;;
(defmacro patched-defpackage (name &rest options)
  (let ((alias-list nil)
        (nickname-list nil)
        (cleaned-options nil))
    ;; process the given options
    (dolist (option-clause options)
      ;; save the nicknames to check for conflicts
      (when (eql (first option-clause) :nicknames)
        (setf nickname-list (rest option-clause)))
      ;; check for aliases
      (if (eql (first option-clause) :alias)
          (dolist (alias-clause (rest option-clause))
            (if (= (length alias-clause) 2)
                (pushnew (cons (first alias-clause) (second alias-clause))
                         alias-list)
                (error "Malformed alias clause: ~A~%" alias-clause)))
          ;; only pass standard CL options to the original DEFPACKAGE
          (pushnew option-clause cleaned-options)))
    (let ((alias-sym (gensym "ALIAS"))
          (package-sym (gensym "PACKAGE")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (package-aliases::%check-package-for-conflicts ',name ',nickname-list)
         (package-aliases::%check-aliases-for-conflicts ',alias-list ',name)
         (let ((,package-sym (package-aliases::original-defpackage
                              ,name
                              ,@cleaned-options)))
           (package-aliases:delete-all-aliases-in-package ,package-sym)
           (dolist (,alias-sym ',alias-list)
             (package-aliases:make-package-alias ,package-sym
                                                 (car ,alias-sym)
                                                 (cdr ,alias-sym)))
           ,package-sym)))))

(defun patched-rename-package (package new-name &optional new-nicknames)
  (let ((old-package (find-package package))
        (new-package nil))
    (package-aliases::%check-package-for-conflicts new-name new-nicknames)
    ;; do the rename
    (setf new-package (original-rename-package package new-name new-nicknames))
    (package-aliases::%redirect-aliases-on-package-rename old-package new-package)
    new-package))

(defun patched-delete-package (package)
  (package-aliases::%maybe-purge-package-aliases (find-package package))
  (package-aliases::original-delete-package package))


;;; arch-tag: 35229BA4-2E6A-11D8-B649-000393D3F814
