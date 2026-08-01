# Repository Guidelines

## Purpose

Orfeus is a fast Common Lisp RAW processor for Olympus PEN-F and OM-1 files. It
has one processing engine and two equal frontends: a scriptable CLI and an FLTK
GUI. Processing behavior must not be implemented only in a frontend.

Build the smallest complete, testable workflow before broadening the product.
Do not add speculative abstractions, unrelated refactors, placeholders, or
silent partial implementations. Record deferred product work explicitly rather
than disguising it as finished behavior.

## Architecture

- Keep decoding, image processing, project configuration, batching, and export
  in the frontend-independent core.
- Keep CLI and FLTK code as thin adapters over the same public core API.
- Store projects, presets, batch jobs, and per-photo overrides as portable
  S-expressions read with `*read-eval*` bound to `nil`.
- Keep native dependencies behind narrow adapters with structured conditions.
- Keep general-purpose components as focused subsystems with narrow APIs and
  dependency boundaries so they can later become standalone free libraries
  without major rewrites. Do not split them out before Orfeus proves the API.
- Preserve source pixels and metadata unless an operation explicitly transforms
  them. Never modify input photographs in place.
- Stream or tile large images where practical. Avoid unnecessary copies,
  consing in pixel loops, and generic arithmetic in measured hot paths.
- Prefer established native libraries for RAW decoding, colour management,
  lens correction, and image encoding when they improve correctness or speed.
- Keep the supported development target Linux x86-64 on SBCL. Use the Nix flake
  as the reproducible development and packaging environment.

## Common Lisp Style

- Define project packages once, `:use` only `#:cl`, and import third-party
  symbols explicitly.
- Use focused files organized by coherent responsibility. Do not create generic
  `misc`, `helpers`, or growing utility dumping grounds.
- Use kebab-case without unclear abbreviations. Prefix entity operations,
  suffix predicates with `-p`, and use `->` for conversions.
- Use `defparameter` for reloadable policy and `defvar` only for process state
  intended to survive reload.
- Prefer `first` and `rest` over `car` and `cdr` in application code.
- Use keyword arguments when a function has four or more parameters.
- Document exported functions, classes, generic functions, macros, and
  conditions.
- Use typed domain conditions with useful reports. Offer restarts for failures
  that callers can reasonably recover from.
- Add declarations only when they clarify an invariant or improve a measured
  hot path. Do not trade correctness for unverified optimization.

## Tests and Verification

- Cover successful and failing paths at external boundaries.
- Test S-expression round trips with `*read-eval*` disabled.
- Verify extracted originals byte-for-byte or against their specified digest.
- Keep small deterministic fixtures in the repository. Never commit personal
  photographs or depend on `/FOTO` in the automated test suite.
- Use local photographs only for explicit integration and performance checks,
  and never alter them.
- Run focused tests while developing and the complete ASDF test system before
  every commit that changes behavior.
- Exercise both frontends against the same processing operations.
- Benchmark representative PEN-F and OM-1 files before claiming performance
  improvements.

## Dependencies and Generated Files

- Declare Lisp dependencies in ASDF and native dependencies in `flake.nix`.
- Pin fetched inputs through `flake.lock`.
- Do not commit build products, extracted ORFs, previews, exports, caches, or
  machine-local configuration.
- Keep credentials and private paths out of source, tests, logs, and fixtures.
- Include third-party LUTs only when their license permits redistribution and
  preserve their attribution and license information.

## Commit Policy

Work directly on `master`. Make frequent, small, coherent commits as soon as
their focused checks pass. Commit messages contain only an imperative title
line, normally under 72 characters. Do not add a body, issue footer, or generated
attribution. Stage only files belonging to the commit and preserve unrelated
work. Push `master` immediately after every commit. Never force-push or rewrite
published history unless the user explicitly requests it.
