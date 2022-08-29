# VSS

[![Build](https://github.com/AdaCore/VSS/workflows/Build/badge.svg)](https://github.com/AdaCore/VSS/actions)
[![codecov](https://codecov.io/gh/AdaCore/VSS/branch/master/graph/badge.svg)](https://codecov.io/gh/AdaCore/VSS)
[![alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/vss.json)](https://alire.ada.dev/crates/vss.html)

A high level string and text processing library.

## The objectives

The objectives of this project are

* To introduce a definite type that represents a string of Unicode characters
  and provides a handy set of operations.
* The API should be encoding independent and allows efficient implementations
  depending on platform/application and avoid extra encoding conversions, e.g.
  an UTF-8 internal representation for Gtk+ applications, UCS-2 for native
  Windows applications and UTF-16 for WebAssembly.
* Besides separating string API from in-memory data representation, it should
  separate string API from input/output stream representation.
* To provide a clear, well-defined semantic for iteration over string
  elements, such as unicode unit, grapheme cluster, etc.
* To avoid string element integer indexes, because this isn't necessarily
  constant time and could mislead about indexed element kind.
* To prefer safe results over raising exceptions where possible.

Warning: This is experimental work in progress, everything is subject to
change. It may be or may be not part of GNATCOLL or standard Ada library
in the future.

Note: Some Ada 2022 features are used in source code. This requires compiler
that supports them.

## Install

### Build from sources
Prefered way to install is to download sources and run

    make all install PREFIX=/path/to/install

### Using `alire`
Or you can use [alire](https://alire.ada.dev/) library manager:

    alr get --build vss

Then you can use it as dependency in the project file:

    with "vss_text.gpr";

For use JSON streaming API:

    with "vss_json.gpr";

## Documentation

* [API Reference](https://adacore.github.io/VSS/)

## Maintainers

[@AdaCore](https://adacore.com/).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/AdaCore/VSS/issues/new)
or submit PRs.

## License

[GPL-3](LICENSE) with [GCC runtime library exception](COPYING.RUNTIME).
