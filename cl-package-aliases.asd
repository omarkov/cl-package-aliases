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

(defpackage :org.entrox.package-aliases.system
  (:use #:cl #:asdf))
(in-package :org.entrox.package-aliases.system)


(defsystem :cl-package-aliases
  :name "Package aliases"
  :author "Oliver Markovic <entrox@entrox.org>"
  :maintainer "Oliver Markovic <entrox@entrox.org>"
  :licence "BSD sans advertising clause"
  :description "Provides per-package nicknames."
  :version "0.9.3"
  :serial t
  :components
  ((:file "package")
   (:file "save-definitions")
   (:file "package-aliases")
   (:file "patch-generic")
   #+allegro (:file "patch-allegro")
   #+cmu (:file "patch-cmucl")
   #+lispworks (:file "patch-lispworks")
   #+openmcl (:file "patch-openmcl")
   #+sbcl (:file "patch-sbcl")))


(defsystem :cl-package-aliases-tests
  :name "Package aliases tests"
  :description "Unit tests for package aliases"
  :serial t
  :depends-on ("cl-package-aliases")
  :components
  ((:file "rt")
   (:file "tests")))


(defmethod perform ((o test-op) (c (eql (find-system 'cl-package-aliases))))
  (operate 'load-op 'cl-package-aliases-tests)
  (or (funcall (intern (symbol-name '#:do-tests)
                       (find-package '#:regression-test)))
      (error "test-op failed.")))


;;; arch-tag: 2FA9DA8F-2E69-11D8-B7C8-000393D3F814
