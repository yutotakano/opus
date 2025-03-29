# opus: Haskell Bindings to libopus

> This is an active fork of previous work at
> [alios/opus](https://github.com/alios/opus)
> with permission granted from the original author to publish the package/code
> under the same name.

Xiph.Org Foundation's Opus Audio codec is a widely used audio codec for VoIP and
streaming. `libopus` is the reference implementation of an encoder and decoder
for this codec, and is available to be used via various package managers
[under the BSD-3 license](https://opus-codec.org/license/).

This Haskell package provides bindings to `libopus`'s encoding and decoding
functionality. It is continuously tested against the official audio vectors in
the project CI (on GitHub).

The package also provides Conduit functions for encoding and decoding, for easy
use in streaming scenarios.

## Usage

1. Add `opus` to your cabal/stack project dependencies
2. Install `libopus` and `pkg-config` (used for finding where libopus is):
   1. **On Windows**: Run the following commands in your MSYS2 environment:
      ```
      pacman -S mingw64/mingw-w64-x86_64-pkg-config
      pacman -S mingw64/mingw-w64-x86_64-opus
      ```
      If you do not know where your MSYS2 environment is, but you installed
      the Haskell toolchain using GHCup, try:
      ```
      ghcup run -m -- pacman -S mingw64/mingw-w64-x86_64-pkg-config
      ghcup run -m -- pacman -S mingw64/mingw-w64-x86_64-opus
      ```
   2. **On MacOS**: Run the following commands:
      ```
      brew install opus
      ```
   3. **On Ubuntu/Debian**: Run the following commands:
      ```
      apt-get install pkg-config
      apt-get install libopus-dev
      ```
3. Import and get going! For example, import `Codec.Audio.Opus.Encoder` to use
   the `opusEncoderCreate` and `opusEncode` functions.

## Development

To develop locally, you will need the `pkg-config` (pre-installed on Mac) and
`libopus` system packages, as described above.

To run tests locally, you will also need the Opus test vectors available within
a directory called `opus_newvectors`. The following command will do just that:

```
curl -L https://opus-codec.org/static/testvectors/opus_testvectors-rfc8251.tar.gz | tar -xz
```

## License

`test/opus_compare.c` was taken as-is from Xiph.Org Foundation under the BSD-3
license.

This project as a whole is licensed under the BSD-3 license. Please see the
LICENSE file for more information.

Copyright (c) 2018 Markus Barenhoff  
Copyright (c) 2021-2022 Yuto Takano  
Copyright (c) 2025-PRESENT Haskell Opus Library Contributors

