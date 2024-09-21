;;;; documentation.lisp — Documentation for Transume
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2024 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Package-specific documentation

(in-package #:transume)

(pax:defsection @transume-api (:title "API reference" :export nil)
  "The Transume protocol consists of two generic functions, SERIALIZE and
DESERIALIZE, which take CLIENT and POLICY parameters.
Users of this protocol fall into two groups: format implementers (i.e.,
serializers and deserializers) and end users (clients).

Format implementers must not specialize CLIENT, lest end users be
constrained in how many formats they can support simultaneously.
Instead, they provide most of their behavior by specializing POLICY, and
to a lesser extent the rest of the parameters other than CLIENT.
POLICY may be a plain symbol or an instance of a class with slots
describing how to convert the data.

Clients may specialize any parameter to customize the serialization or
deserialization of their data.
They can even specialize POLICY, though such specialization should avoid
implementing formats, rather, it could serve to combine or chain
policies, e.g., for converting one format into another into another."
  (serialize generic-function)
  (deserialize generic-function))

;;;* Definition of documentation package

(uiop:define-package #:transume/documentation
  (:use #:common-lisp)
  (:import-from #:transume #:@transume-api)
  (:import-from #:mgl-pax))
(in-package #:transume/documentation)

;;;* Manual

(pax:defsection @transume-manual (:title "Transume Manual")
  ("transume" asdf:system)
  (@transume-introduction pax:section)
  (@transume-installing pax:section)
  (@transume-api pax:section)
  (@transume-similar-libraries pax:section)
  (@transume-acknowledgments pax:section))

;;;** Introduction

(pax:defsection @transume-introduction (:title "Introduction")
  "Transume is a Common Lisp library which consists of a protocol for
serializing and deserializing data and maybe eventually a collection of
external library wrappers implementing this protocol.
The protocol was designed with extensibility in mind, namely via the
flexible CLIENT parameter (as used in [Trucler][Trucler] and other
[s-expressionists][s-expressionists] projects), which allows end
users (clients) of the protocol to customize how their data will be
serialized or deserialized.

The ultimate goal of Transume is for as many serializers and
deserializers as possible to be exposed through its extensible protocol.
Ideally, external libraries would implement the protocol, if not using
the functions directly as part of their protocol — perhaps to avoid the
additional dependency or because the API is incompatible — as a separate
package, possibly named with the “-Transume” suffix.
Failing this, Transume may house simple wrappers for external, including
foreign, libraries.
Dedicated implementations of serializers and deserializers are
explicitly out of scope for Transume.

Transume is currently under active development and its API is unstable,
but it follows the [StableVer][StableVer] versioning scheme, and the
stability of each API component is marked in the associated
documentation, e.g., docstrings.

[Trucler]: https://github.com/s-expressionists/trucler
[s-expressionists]: https://github.com/s-expressionists
[StableVer]: https://gist.github.com/brandonbloom/465625acaf0120354614e7fc0c117c62"
  (@transume-background pax:section))

(pax:defsection @transume-background (:title "Background")
  "The word [transume][Wiktionary/transume] means “to change, convert”.

[Wiktionary/transume]: https://en.wiktionary.org/wiki/transume")

;;;** Installing

(pax:defsection @transume-installing (:title "Installing")
  "Transume is not available on [Quicklisp][Quicklisp], so you will need to
clone it to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/transume
```

Transume has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html")

;;;** Similar libraries

(pax:defsection @transume-similar-libraries (:title "Similar libraries")
  "[NJSON][NJSON] exposes a uniform interface over various JSON libraries,
similar in concept to Transume, though limited to JSON and also to
loading only one backend (wrapped library) at a time.

[NJSON]: https://github.com/atlas-engineer/njson")

;;;** Acknowledgments

(pax:defsection @transume-acknowledgments (:title "Acknowledgments")
  "I would like to thank [Tarn W. “yitzi” Burton][yitzchak] for
discussions about the design of Transume.

[yitzchak]: https://github.com/yitzchak")
