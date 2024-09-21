;;;; transume.lisp — Protocol for serializing and deserializing data
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2024 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Package definition

(uiop:define-package #:transume
  (:use #:common-lisp)
  (:export #:serialize #:deserialize))
(in-package #:transume)

;;;* Implementation

(defgeneric serialize (client object output metadata policy)
  (:argument-precedence-order client policy object output metadata)
  (:documentation "Serialize OBJECT to OUTPUT according to CLIENT and POLICY.
Return OBJECT.

METADATA may be additional information associated to OBJECT, POLICY is a
representation of the serializer, and CLIENT is a parameter by which end
users may customize the standard behavior.
The implementation of the serializer must not specialize CLIENT.

(Stability: beta)"))

;;; A possible convention for RESULT-TYPE could be T for whatever is
;;; parsed, and NIL to validate the input.
(defgeneric deserialize (client result-type input schema policy)
  (:argument-precedence-order client policy result-type input schema)
  (:documentation "Deserialize INPUT as RESULT-TYPE according to CLIENT and POLICY.
Return the deserialized object.

SCHEMA may be additional information associated to INPUT, POLICY is a
representation of the deserializer, and CLIENT is a parameter by which
end users may customize the standard behavior.
The implementation of the deserializer must not specialize CLIENT.

(Stability: beta)"))
