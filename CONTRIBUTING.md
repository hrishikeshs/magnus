# Contributing to Magnus

Thanks for helping improve Magnus. This package is Magit-inspired Emacs Lisp:
small, focused changes with tests land fastest.

## Prerequisites

- Emacs 28.1+ (`emacs --version`)
- Network access once to install ELPA dependencies used by CI/tests

Install the same test dependencies CI uses:

```sh
emacs --batch -Q -l test/install-ci-dependencies.el
```

## Verify locally

From the repository root:

```sh
make test          # ERT suite (batch)
make lint          # project lint.el checks
make lint-compile  # byte-compile all package files
make package-lint  # package-lint metadata check
```

Please run at least `make test` before opening a pull request. If you touch
package headers or public APIs, also run `make package-lint`.

## Pull requests

1. Fork and create a branch from `master`.
2. Keep the change to one concern (feature, fix, docs, or test).
3. Prefer adding or extending an ERT test under `test/` when behavior changes.
4. Open a PR against `hrishikeshs/magnus:master` with a short summary of the
   problem, the change, and the verification commands you ran.

## Where to look

| Area | Start here |
| --- | --- |
| Status buffer / keys | `magnus-status.el`, `docs/reference.md` |
| Attention queue | `magnus-attention.el`, `test/magnus-attention-tests.el` |
| Providers (Claude / Codex) | `magnus-provider.el`, `magnus-provider-*.el` |
| Reviews | `magnus-review*.el`, `docs/reviews.md` |
| Architecture / trust | `docs/architecture.md` |

Questions and design discussion are welcome on the PR or by opening an issue.
