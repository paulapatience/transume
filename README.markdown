<!--
SPDX-FileCopyrightText: Copyright (c) 2024 Paul A. Patience <paul@apatience.com>
SPDX-License-Identifier: MIT
-->

# Transume

Protocol for serializing and deserializing data in Common Lisp.

## Description

Transume consists of a protocol of two generic functions for serializing
and deserializing data and maybe eventually a collection of external
library wrappers implementing this protocol.
The protocol is:

```common-lisp
(defgeneric serialize (client object output metadata policy)
  (:argument-precedence-order client policy object output metadata))

(defgeneric deserialize (client result-type input schema policy)
  (:argument-precedence-order client policy result-type input schema))
```

The main advantage of this protocol is that it provides the especially
flexible CLIENT parameter (as used in [Trucler][] and other
[s-expressionists][] projects), which allows end users (clients) of the
protocol to customize how their data will be serialized or deserialized.
The CLIENT parameter is left unspecialized by serializers and
deserializers — lest clients of the protocol be constrained in how many
formats they can support simultaneously — which instead specialize the
POLICY parameter and may be further informed by the output METADATA and
input SCHEMA.
See the [manual](transume.markdown) for more information.

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
but it follows the [StableVer][] versioning scheme, and the stability of
each API component is marked in the associated documentation, e.g.,
docstrings.

[Trucler]: https://github.com/s-expressionists/trucler
[s-expressionists]: https://github.com/s-expressionists
[StableVer]: https://gist.github.com/brandonbloom/465625acaf0120354614e7fc0c117c62

## Installing

Transume is not available on [Quicklisp][], so you will need to clone it
to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/transume
```

Transume has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html

## Documentation

Transume is documented in its manual, which consists of an embellished
version of this README supplemented with the API reference.
It is available in [Markdown form](transume.markdown) and in PDF form,
the latter of which is distributed only alongside the release archive.

The manual is regenerated only at each release, so in order to consult
the latest version, you need to build it manually:

```sh
make markdown pdf
```

Building the manual requires [MGL-PAX][], [Fanion][] and
[Pathname-Utils][], and building the PDF manual further requires
[Pandoc][], LuaLaTeX and suitable LaTeX packages.

[MGL-PAX]: https://melisgl.github.io/mgl-pax-world/pax-manual.html
[Fanion]: https://git.sr.ht/~paulapatience/fanion
[Pathname-Utils]: https://shinmera.github.io/pathname-utils/
[Pandoc]: https://pandoc.org/

## Roadmap

The protocol is unlikely to ever change, but wrapper packages may be
added in the future.

## See also

[NJSON][] exposes a uniform interface over various JSON libraries,
similar in concept to Transume, though limited to JSON and also to
loading only one backend (wrapped library) at a time.

[NJSON]: https://github.com/atlas-engineer/njson

## Acknowledgments

I would like to thank [Tarn W. “yitzi” Burton][yitzchak] for
discussions about the design of Transume.

[yitzchak]: https://github.com/yitzchak

## License

This project is licensed under the [MIT license (Expat)](LICENSE).

Unless you explicitly state otherwise, any contribution intentionally
submitted by you for inclusion in this project shall be licensed as
above, without any additional terms or conditions.
