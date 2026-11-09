# Contributing to haal

## Branching model

- `main` only ever receives merges from `develop`, each one a release.
  Nothing is committed to `main` directly.
- `develop` is the integration branch. All work branches off `develop` and
  is merged back into it via a pull request.

So to contribute:

1. Branch off `develop`.
2. Open a pull request against `develop`.
3. Once it passes CI and is approved, it gets merged into `develop` and
   ships in the next release.

### Branch naming

Name your branch `<type>/<short-description>`, using one of:

- `feature/<short-description>` — new functionality
- `fix/<short-description>` — bug fixes (reference an issue number if there
  is one, e.g. `fix/42-empty-characterizing-set`)
- `docs/<short-description>` — documentation only
- `chore/<short-description>` — tooling, CI, dependencies, and other
  changes that don't affect library behaviour

`<short-description>` is a few kebab-case words, e.g. `feature/dot-parser`.

## Making changes

- Format code with [fourmolu](https://github.com/fourmolu/fourmolu)
  (`fourmolu.yaml` holds the project's style).
- Keep `-Wall` (and the rest of the warnings in `package.yaml`) clean.
- Add or update tests under `test/` for any behavioural change.
- Add an entry under `## Unreleased` in `CHANGELOG.md`, in the appropriate
  `### Added` / `### Changed` / `### Fixed` / `### Removed` section,
  following [Keep a Changelog](https://keepachangelog.com/en/1.0.0/). Mark
  breaking changes as `### Changed (breaking)` or `### Removed (breaking)`.
  Skip this for changes that don't affect the library's users (e.g. CI-only
  changes).

## Before opening a pull request

Run the test suite locally:

```sh
stack test
```

If your change touches code with a LiquidHaskell spec, also check it
verifies:

```sh
stack build haal:lib --flag haal:liquid
```

## Pull requests

- Target `develop`.
- Describe what changed and why.
- All CI status checks must pass before a PR is merged.
