# tagless-final

A Haskell project demonstrating the tagless final style for abstracting over effects, with a focus on file system operations and testability.

## Features
- Tagless final encoding for file system effects (`MonadFS`)
- In-memory and IO implementations
- Unit tests using HUnit
- Testable core logic for searching tokens in files

## Usage

### Build and Run

```
cabal build
cabal run tagless-final -- <directory> <token>
```

### Run Tests

```
cabal test --test-show-details=direct
```

## Project Structure
- `app/Main.hs`: Core logic and effect type classes
- `test/MainTest.hs`: Test suite for core logic
- `tagless-final.cabal`: Project configuration

