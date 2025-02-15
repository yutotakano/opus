# ChangeLog for `opus`

### Unreleased Changes

- nothing yet!

### 0.3.0.0 — 2025 February

- **[breaking]** Update Cabal package description file schema to 3.0 (can no longer run with low Cabal versions)
- **[breaking]** Use opus.h for includes instead of opus/opus.h for better Windows/Mingw compatibility
- **[breaking]** Use pkg-config on all platforms including Windows
- Add CI to run tests on every pull request on GitHub
- Modify test suite to remove dependency on `opus-tools` package (instead we now FFI into a local opus_compare.c)
- Add Haddock documentation to all modules, values, and functions.

### 0.2.1.0 — 2025 February

- Remove stack from project as Cabal is enough and reduces complexity
- Received permission from alios (the original author) to release this package under the original name
- Update project synopsis, description, links, maintainer info etc in .cabal file
- Remove `hspec` from library dependency
- Fix wrong include path for opus.h in hsc file (on Windows)

### 0.2.0.0 — 2022 May

- Decoder and decoder conduit implemented
- `opus` is forked from alios (the original author) due to inactivity
- Add a test suite for decoding mono and stereo audio
- Migrate from `lens` to `microlens` for lighter dependency

### 0.1.0.0 — 2018 July

- Encoder and encoder conduit implemented
