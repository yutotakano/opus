# ChangeLog for `opus`

## Unreleased Changes

## 0.2.1.0

- Remove stack from project as Cabal is enough and reduces complexity
- Received permission from alios (the original author) to release this package under the original name
- Update project synopsis, description, links, maintainer info etc in .cabal file
- Remove `hspec` from library dependency
- Fix wrong include path for opus.h in hsc file (on Windows)

## 0.2.0.0

- Decoder and decoder conduit implemented
- `opus` is forked from alios (the original author) due to inactivity
- Add a test suite for decoding mono and stereo audio
- Migrate from `lens` to `microlens` for lighter dependency

## 0.1.0.0

- Encoder and encoder conduit implemented
