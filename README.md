# ppad-base64

[![](https://img.shields.io/hackage/v/ppad-base64?color=blue)](https://hackage.haskell.org/package/ppad-base64)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-base64-lightblue)](https://docs.ppad.tech/base64)

Pure base64 encoding & decoding on strict ByteStrings.

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

The aim is best-in-class performance for pure, highly-auditable Haskell
code. We could go slightly faster by using direct allocation and writes,
but we get pretty close to the best impure versions with only builders.

Current benchmark figures on a 1024-byte input on an Apple M4 MacBook Air,
GHC 9.10.3 with the LLVM backend, look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking ppad-base64/encode
  time                 2.279 μs   (2.253 μs .. 2.316 μs)
                       0.999 R²   (0.998 R² .. 1.000 R²)
  mean                 2.284 μs   (2.270 μs .. 2.308 μs)
  std dev              74.77 ns   (50.21 ns .. 124.4 ns)

  benchmarking ppad-base64/decode
  time                 649.2 ns   (637.2 ns .. 659.0 ns)
                       0.998 R²   (0.997 R² .. 0.999 R²)
  mean                 618.5 ns   (611.8 ns .. 625.5 ns)
  std dev              29.46 ns   (25.76 ns .. 35.06 ns)
```

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
