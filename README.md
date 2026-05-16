# ppad-base64

[![](https://img.shields.io/hackage/v/ppad-base64?color=blue)](https://hackage.haskell.org/package/ppad-base64)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-base64-lightblue)](https://docs.ppad.tech/base64)

Fast base64 encoding & decoding on strict ByteStrings.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Data.ByteString.Base64 as B64
  >
  > -- simple base64 encoding and decoding
  > B64.encode "hello world"
  "aGVsbG8gd29ybGQ="
  >
  > B64.decode "aGVsbG8gd29ybGQ="
  Just "hello world"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/base64](https://docs.ppad.tech/base64).

## Performance

The aim is best-in-class performance. Current benchmark figures on 1kb
inputs on an M4 Silicon MacBook Air, where we avail of hardware
acceleration via ARM NEON intrinsics, look like (use `cabal bench` to
run the benchmark suite):

```
  benchmarking ppad-base64/encode
  time                 102.0 ns   (101.9 ns .. 102.2 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 102.0 ns   (101.9 ns .. 102.1 ns)
  std dev              386.6 ps   (313.4 ps .. 521.5 ps)

  benchmarking ppad-base64/decode
  time                 160.3 ns   (160.3 ns .. 160.4 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 160.3 ns   (160.2 ns .. 160.4 ns)
  std dev              242.8 ps   (201.8 ps .. 301.2 ps)
```

You should compile with the 'llvm' flag for maximum performance.

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-base64
```

to get a REPL for the main library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
