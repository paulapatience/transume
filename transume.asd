;;;; transume.asd — ASDF system definition for Transume
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2024 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Principal systems

(defsystem "transume"
  :version (:read-file-line "Makefile" :at 10)
  :description "Protocol for serializing and deserializing data."
  :author "Paul A. Patience"
  :mailto "paul@apatience.com"
  :license "MIT (Expat)"
  :homepage "https://git.sr.ht/~paulapatience/transume"
  :source-control (:git "https://git.sr.ht/~paulapatience/transume")
  :class :package-inferred-system
  :depends-on ("transume/transume"))

(defsystem "transume/documentation"
  :version (:read-file-line "Makefile" :at 10)
  :description "Documentation for Transume."
  :author "Paul A. Patience"
  :mailto "paul@apatience.com"
  :license "MIT (Expat)"
  :depends-on ("transume" "mgl-pax")
  :components ((:file "documentation")))
