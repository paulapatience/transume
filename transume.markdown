<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# Transume Manual

## Table of Contents

- [1 The transume ASDF System][4a51]
- [2 Introduction][0324]
    - [2.1 Background][a204]
- [3 Installing][0787]
- [4 API reference][06af]
- [5 Similar libraries][063e]
- [6 Acknowledgments][730e]

###### \[in package TRANSUME/DOCUMENTATION\]
<a id="x-28-22transume-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

## 1 The transume ASDF System

- Version: 0.1
- Description: Protocol for serializing and deserializing data.
- Licence: MIT (Expat)
- Author: Paul A. Patience
- Mailto: [paul@apatience.com](mailto:paul@apatience.com)
- Homepage: [https://git.sr.ht/~paulapatience/transume](https://git.sr.ht/~paulapatience/transume)
- Source control: [GIT](https://git.sr.ht/~paulapatience/transume)

<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 2 Introduction

Transume is a Common Lisp library which consists of a protocol for
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

[StableVer]: https://gist.github.com/brandonbloom/465625acaf0120354614e7fc0c117c62 


<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>

### 2.1 Background

The word [transume][Wiktionary/transume] means “to change, convert”.

[Wiktionary/transume]: https://en.wiktionary.org/wiki/transume 


<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-INSTALLING-20MGL-PAX-3ASECTION-29"></a>

## 3 Installing

Transume is not available on [Quicklisp][Quicklisp], so you will need to
clone it to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/transume
```

Transume has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html 


<a id="x-28TRANSUME-3A-40TRANSUME-API-20MGL-PAX-3ASECTION-29"></a>

## 4 API reference

###### \[in package TRANSUME\]
The Transume protocol consists of two generic functions, [`SERIALIZE`][9eb5] and
[`DESERIALIZE`][61b1], which take `CLIENT` and `POLICY` parameters.
Users of this protocol fall into two groups: format implementers (i.e.,
serializers and deserializers) and end users (clients).

Format implementers must not specialize `CLIENT`, lest end users be
constrained in how many formats they can support simultaneously.
Instead, they provide most of their behavior by specializing `POLICY`, and
to a lesser extent the rest of the parameters other than `CLIENT`.
`POLICY` may be a plain symbol or an instance of a class with slots
describing how to convert the data.

Clients may specialize any parameter to customize the serialization or
deserialization of their data.
They can even specialize `POLICY`, though such specialization should avoid
implementing formats, rather, it could serve to combine or chain
policies, e.g., for converting one format into another into another.

<a id="x-28TRANSUME-3ASERIALIZE-20GENERIC-FUNCTION-29"></a>

- [generic-function] **SERIALIZE** *CLIENT OBJECT OUTPUT METADATA POLICY*

    Serialize `OBJECT` to `OUTPUT` according to `CLIENT` and `POLICY`.
    Return `OBJECT`.
    
    `METADATA` may be additional information associated to `OBJECT`, `POLICY` is a
    representation of the serializer, and `CLIENT` is a parameter by which end
    users may customize the standard behavior.
    The implementation of the serializer must not specialize `CLIENT`.
    
    (Stability: beta)

<a id="x-28TRANSUME-3ADESERIALIZE-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DESERIALIZE** *CLIENT RESULT-TYPE INPUT SCHEMA POLICY*

    Deserialize `INPUT` as `RESULT-TYPE` according to `CLIENT` and `POLICY`.
    Return the deserialized object.
    
    `SCHEMA` may be additional information associated to `INPUT`, `POLICY` is a
    representation of the deserializer, and `CLIENT` is a parameter by which
    end users may customize the standard behavior.
    The implementation of the deserializer must not specialize `CLIENT`.
    
    (Stability: beta)

<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-SIMILAR-LIBRARIES-20MGL-PAX-3ASECTION-29"></a>

## 5 Similar libraries

[NJSON][NJSON] exposes a uniform interface over various JSON libraries,
similar in concept to Transume, though limited to JSON and also to
loading only one backend (wrapped library) at a time.

[NJSON]: https://github.com/atlas-engineer/njson 


<a id="x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-ACKNOWLEDGMENTS-20MGL-PAX-3ASECTION-29"></a>

## 6 Acknowledgments

I would like to thank [Tarn W. “yitzi” Burton][yitzchak] for
discussions about the design of Transume.

[yitzchak]: https://github.com/yitzchak 


  [0324]: #x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [063e]: #x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-SIMILAR-LIBRARIES-20MGL-PAX-3ASECTION-29 "Similar libraries"
  [06af]: #x-28TRANSUME-3A-40TRANSUME-API-20MGL-PAX-3ASECTION-29 "API reference"
  [0787]: #x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-INSTALLING-20MGL-PAX-3ASECTION-29 "Installing"
  [4a51]: #x-28-22transume-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"transume" ASDF/SYSTEM:SYSTEM'
  [61b1]: #x-28TRANSUME-3ADESERIALIZE-20GENERIC-FUNCTION-29 "TRANSUME:DESERIALIZE GENERIC-FUNCTION"
  [730e]: #x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-ACKNOWLEDGMENTS-20MGL-PAX-3ASECTION-29 "Acknowledgments"
  [9eb5]: #x-28TRANSUME-3ASERIALIZE-20GENERIC-FUNCTION-29 "TRANSUME:SERIALIZE GENERIC-FUNCTION"
  [a204]: #x-28TRANSUME-2FDOCUMENTATION-3A-40TRANSUME-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
