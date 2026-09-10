# Getting started with Magnus

[Back to README](../README.md) · [Reviews](reviews.md) ·
[Reference](reference.md) · [Troubleshooting](troubleshooting.md)

Magnus is a hands-on Emacs workspace for Claude Code and Codex. It launches
each provider's real terminal UI and adds persistent identity, coordination,
status, and independent review around it.

## 1. Prepare Emacs and the provider CLIs

Magnus requires:

- Emacs 28.1 or newer;
- `vterm` 0.0.2 or newer;
- `transient` 0.4.0 or newer;
- `magit-section` 3.3.0 or newer;
- Git;
- Claude Code, Codex CLI, or both.

Install each provider CLI from its official distribution, launch it once in a
normal terminal, and complete its authentication flow before asking Magnus to
start it. Magnus uses the CLI's existing authentication and configuration.

Only one provider is necessary for ordinary interactive work. Install both if
you want the default review policy, which chooses the provider opposite the
author.

## 2. Install Magnus

### MELPA

```elisp
(use-package magnus
  :ensure t
  :bind (("C-c m" . magnus)
         ("C-c M" . magnus-create-instance)))
```

Or use `M-x package-install RET magnus RET`.

### package-vc (Emacs 29+)

```text
M-x package-vc-install RET https://github.com/hrishikeshs/magnus RET
```

```elisp
(unless (package-installed-p 'magnus)
  (package-vc-install "https://github.com/hrishikeshs/magnus"))
```

### straight.el

```elisp
(straight-use-package
 '(magnus :type git :host github :repo "hrishikeshs/magnus"))
```

### quelpa

```elisp
(quelpa '(magnus :fetcher github :repo "hrishikeshs/magnus"))
```

A clean first install does not require a restart. Restart when the current
Emacs session has already loaded another Magnus version; its structures,
timers, and functions must not be mixed with the replacement.
`M-x magnus-upgrade` performs a guarded reinstall and then asks for that
restart.

## 3. Configure provider commands

Magnus uses `claude` and `codex` from `exec-path` by default. Override them when
your installation lives elsewhere:

```elisp
(setq magnus-claude-executable "/path/to/claude")
(setq magnus-codex-executable "/path/to/codex")
```

Each value may also be an established command prefix with arguments. Magnus
preserves those arguments when it launches interactive sessions, session
probes, and headless work.

On macOS, GUI Emacs sometimes inherits a smaller `PATH` than an interactive
shell. Confirm that the relevant provider command appears in `exec-path`, or
use an absolute path as above.

## 4. Run Doctor

Run `M-x magnus-doctor`. Doctor performs read-only checks for:

- the Emacs version and required libraries;
- Claude Code, Codex CLI, and Git;
- Magnus's durable state paths and permissions;
- registered agent directories;
- coordination watchers for active projects.

Missing optional providers are warnings. A missing provider only disables that
provider's agents and reviews; at least one usable provider is required.

Doctor is also available as `? D` from the Magnus status buffer.

## 5. Create the first agents

Run `M-x magnus` to open `*magnus*`.

- Press `c` to create a Claude Code agent.
- Press `X` to create a Codex agent.
- Press `RET` on an agent to visit its terminal.
- Press `?` anywhere in the status buffer to see the complete dispatcher.

Magnus assigns a friendly name such as `swift-fox` and a durable UUID. The
name is used for display, memory, and human coordination; the UUID keeps the
identity stable across rename, archive, and resurrection.

Both providers run directly in `vterm`. Their normal approval prompts, slash
commands, terminal composers, and streaming output remain available. In a
Magnus terminal, `C-g` sends Escape to the provider because Emacs normally
consumes `ESC` as a Meta prefix.

## 6. Read the status buffer

The status buffer groups active and archived agents, coordination state,
review activity, and completed review lineages. Agent rows show provider,
lifecycle, directory, health, attention, active work, and any running review
badge.

The minibuffer follows point with contextual hints. Direct keys cover the most
common actions; `?` exposes everything through a Transient menu.

The typical hands-on loop is:

1. create two agents for the same project;
2. visit each native TUI and give it focused work;
3. return to `*magnus*` to see who is active or needs attention;
4. use ordinary conversation to redirect an agent;
5. commit completed work and request an independent review with `v RET`.

Reviews require a Git repository, a clean author worktree, and committed work.
See [Independent reviews](reviews.md) for the full flow.

## Provider capability summary

| Capability | Claude Code | Codex |
|------------|-------------|-------|
| Native interactive `vterm` UI | yes | yes |
| Durable identity and provider-session resume | yes | when captured |
| Archive and resurrection | yes | yes |
| Shared coordination, attention, and health | yes | yes |
| Trace viewer | recorded thinking and responses | visible journals and responses |
| Live process suspend/resume | yes | no |
| User-invoked fire-and-forget headless task | yes | no |
| Review author or reviewer | yes | yes |

Codex's encrypted internal reasoning is not available to Magnus. The trace
viewer shows the visible engineering journal and response records that Codex
writes locally.

## Next steps

- Learn the [independent review workflow](reviews.md).
- Read the [command and customization reference](reference.md).
- Understand the [architecture and trust boundaries](architecture.md).
- Check [troubleshooting](troubleshooting.md) when setup or review fails.
