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

Benchmark figures will be added after running the suite on the standard
VPS (use `cabal bench` to run the benchmark suite).

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
