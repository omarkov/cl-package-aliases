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

(defpackage :org.entrox.package-aliases.tests
  (:use :cl :regression-test :package-aliases))
(in-package :org.entrox.package-aliases.tests)

(rem-all-tests)

(defmacro expect-error (&body body)
  `(handler-case (progn ,@body)
    (error () t)
    (:no-error nil)))

(defpackage :test-package-a)

(defpackage :test-package-b
  (:alias (:test-package-a :tpa)))

(defpackage :test-package-c
  (:alias (:test-package-a :tpa)
          (:test-package-b :tpb)))

(defpackage :test-package-d)

(defpackage :test-package-e
  (:use :cl)
  (:nicknames :test-package-e-nick)
  (:alias (:test-package-c :tpc)))


;;;
;;; MAKE-PACKAGE-ALIAS
;;;

(deftest make-package-alias.1
  (progn
    (make-package-alias :test-package-d :test-package-c :tpc)
    (length (package-alias-list :test-package-d)))
  1)

;; duplicate
(deftest make-package-alias.2
  (progn
    (make-package-alias :test-package-d :test-package-c :tpc)
    (length (package-alias-list :test-package-d)))
  1)

(deftest make-package-alias.3
  (expect-error
   (make-package-alias :test-package-d :test-package-c :cl))
  t)

(deftest make-package-alias.4
  (expect-error
   (make-package-alias :test-package-d :test-package-c :common-lisp-user))
  t)

(deftest make-package-alias.5
  (expect-error
   (make-package-alias nil nil nil))
  t)


;;;
;;; PACKAGE-ALIAS-EQUAL
;;;

(deftest package-alias-equal.1
  (let ((test-alias-1 (package-aliases::%make-package-alias
                       :from (find-package :test-package-a)
                       :to "A"))
        (test-alias-2 (package-aliases::%make-package-alias
                       :from (find-package :test-package-a)
                       :to "A")))
    (package-alias-equal test-alias-1 test-alias-2))
  t)

(deftest package-alias-equal.2
  (let ((test-alias-1 (package-aliases::%make-package-alias
                       :from (find-package :test-package-a)
                       :to "A"))
        (test-alias-2 (package-aliases::%make-package-alias
                       :from (find-package :test-package-b)
                       :to "B")))
    (package-alias-equal test-alias-1 test-alias-2))
  nil)

(deftest package-alias-equal.3
  (expect-error
   (package-alias-equal :foo :bar))
  t)


;;;
;;; PACKAGE-ALIAS-LIST
;;;

(deftest package-alias-list.1
  (package-alias-list :test-package-a)
  nil)

(deftest package-alias-list.2
  (let ((alias-list (package-alias-list :test-package-b))
        (test-alias (package-aliases::%make-package-alias
                     :from (find-package :test-package-a)
                     :to "TPA")))
    (package-alias-equal (first alias-list) test-alias))
  t)

(deftest package-alias-list.3
  (length (package-alias-list :test-package-c))
  2)

(deftest package-alias-list.4
  (expect-error
   (package-alias-list :a-nonexistent-package))
  t)

(deftest package-alias-list.5
  (expect-error
   (package-alias-list nil))
  t)


;;;
;;; PACKAGE-ALIASED-BY-LIST
;;;

(deftest package-aliased-by-list.1
  (package-aliased-by-list :test-package-d)
  nil)

(deftest package-aliased-by-list.2
  (length (package-aliased-by-list :test-package-b))
  1)

(deftest package-aliased-by-list.3
  (length (package-aliased-by-list :test-package-a))
  2)

(deftest package-aliased-by-list.4
  (expect-error
   (package-aliased-by-list :a-nonexistent-package))
  t)

(deftest package-aliased-by-list.5
  (expect-error
   (package-aliased-by-list nil))
  t)

;;;
;;; RENAME-PACKAGE
;;;

(deftest rename-package.1
  (progn
    (rename-package :test-package-a :test-package-a2)
    (let ((alias (first (package-alias-list :test-package-b))))
      (if (eql (package-alias-from alias) (find-package :test-package-a2))
          t
        nil)))
  t)

(deftest rename-package.2
  (expect-error
   (rename-package :test-package-a2 :tpb))
  t)

;; and backwards..
(deftest rename-package.3
  (progn
    (rename-package :test-package-a2 :test-package-a)
    (let ((alias (first (package-alias-list :test-package-b))))
      (if (eql (package-alias-from alias) (find-package :test-package-a))
          t
        nil)))
  t)


;;;
;;; DELETE-PACKAGE
;;;

(deftest delete-package.1
  (expect-error
   (delete-package :test-package-a))
  t)

(deftest delete-package.2
  (delete-package :test-package-d)
  t)


;;;
;;; MAKE-PACKAGE
;;;

(deftest make-package.1
  (expect-error
   (make-package :tpa))
  t)


;;;
;;; DEFPACKAGE
;;;

(deftest defpackage.1
  (let ((p (find-package :test-package-e)))
    (and (= (length (package-nicknames p)) 1)
         (= (length (package-use-list p)) 1)
         (= (length (package-alias-list p)) 1)))
  t)



;;; arch-tag: 7B7DCBA2-2E6A-11D8-8437-000393D3F814
