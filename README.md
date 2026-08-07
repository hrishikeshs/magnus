# Magnus

[![MELPA](https://melpa.org/packages/magnus-badge.svg)](https://melpa.org/#/magnus)
[![CI](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml/badge.svg)](https://github.com/hrishikeshs/magnus/actions/workflows/ci.yml)

A magit-inspired interface for managing multiple Claude Code and Codex instances within Emacs.

Run multiple AI agents in parallel, let them communicate to avoid conflicts,
and handle their permission requests one at a time.

[Website](https://hrishikeshs.github.io/magnus/) ·
[Getting started](docs/getting-started.md) ·
[Command reference](docs/reference.md) ·
[Architecture](docs/architecture.md)

## Screenshots

**Two agents working side by side in their own `vterm` buffers, with Magnus
keeping the team visible:**

![Two Claude Code agents working in separate vterm buffers beside Magnus](site/assets/magnus-agents.webp)

**Named agents, active work, attention, and coordination in one buffer:**

![Magnus status buffer with Claude Code and Codex agents](site/assets/magnus-status.webp)

## Why Magnus?

When working with Claude Code, you often want multiple agents working simultaneously:
- One agent refactoring the auth module
- Another writing tests
- A third updating documentation

But this creates problems:
- **File conflicts**: Agents might edit the same files
- **Context sharing**: How do agents know what others are doing?
- **Permission chaos**: Multiple agents asking for input at once
- **Independent review**: How do you get one model to review another model's work?

Magnus solves all of this.

## Features

### Native Claude Code and Codex agents

Each agent runs in its provider's full native TUI inside a `vterm` buffer.
Streaming output, approval prompts, slash commands, and the terminal composer
keep working as they do outside Emacs. Magnus adds orchestration around those
interfaces instead of replacing them with another chat UI.

### Other coding agents

Magnus ships with Claude Code and Codex support, but the provider layer is
open. If you want to use another agent, you just need to write an adapter for
it. Register the lifecycle, messaging, trace, and headless operations that its
CLI supports; Magnus's shared layers continue to provide identity, persistence,
coordination, attention, and status.

See [`magnus-provider.el`](magnus-provider.el) for the small dispatch interface
and [`magnus-provider-codex.el`](magnus-provider-codex.el) for a complete
interactive and headless adapter.

### Instance Management

Create, visit, steer, rename, archive, and resurrect named agents such as
`swift-fox` and `keen-owl`. Each agent has its own terminal and working
directory. Magnus preserves provider sessions when they are available, so an
archived collaborator can return with its conversation intact.

### Agent Coordination

Agents communicate through a shared `.magnus-coord.md` file:

```markdown
## Active Work
| Agent | Area | Status | Files |
|-------|------|--------|-------|
| swift-fox | auth module | in-progress | src/auth.ts |
| keen-owl | api tests | in-progress | tests/api/*.test.ts |

## Discoveries
- The user API returns 404 for deleted users — handle both cases (swift-fox)

## Log
[10:30] swift-fox: Starting auth work. I will touch src/auth.ts.
[10:31] keen-owl: Got it. I will stay in tests/api/.
```

Agents are instructed to check in, declare what they are changing, share
discoveries, and notify one another with `@name` mentions. Magnus keeps the
journal lean and surfaces its activity in the status buffer.

This is advisory coordination — agents are instructed to follow the protocol, and Claude is good at it.
Codex receives equivalent provider-neutral instructions.

### Attention, health, and steering

- Press `m` in the status buffer to send an agent a message.
- Agents waiting for permission or input enter one attention queue; press `a`
  to visit the next one.
- Health symbols show which terminals are active, stale, stuck, or dead.
- Common safe operations can be auto-approved with a customizable allowlist.

Magnus is built for a programmer who wants to remain in the loop: keep two
agents side by side, move between their native terminals, and redirect them as
the work evolves.

### Shared knowledge and context

Each project can have a shared scratch buffer for notes, links, tickets, and
other source material. Agent memories and coordination discoveries survive
individual sessions. Press `t` to follow a provider's recorded trace: Claude
thinking blocks where available, or Codex's visible engineering journals and
messages—not encrypted internal reasoning.

For self-contained jobs, Magnus can also run fire-and-forget Claude tasks as
headless background processes. Interactive agents remain the main workflow.

### Independent cross-provider review

When an agent finishes a committed unit of work, put point on it in `*magnus*`
and press `v RET`. Magnus asks the author for the exact commits, chooses a
reviewer using its existing expertise matching, and uses the opposite provider
by default. Uncommitted work is rejected with a prompt to ask the author to
commit first.

The result opens as a folding, Magit-style diff with findings anchored to the
reviewed snapshot. Follow-up rounds retain the reviewer's identity and session,
so author and reviewer can continue until the findings are resolved.

![A completed Magnus review over exact committed evidence](site/assets/magnus-review.webp)

See [Independent reviews](docs/reviews.md) for the review lifecycle, reader
commands, and evidence guarantees.

## Requirements

- Emacs 28.1 or newer
- [vterm](https://github.com/akermu/emacs-libvterm) 0.0.2 or newer
- [transient](https://github.com/magit/transient) 0.4.0 or newer
- [magit-section](https://github.com/magit/magit) 3.3.0 or newer
- [Claude Code](https://docs.anthropic.com/en/docs/claude-code),
  [Codex CLI](https://github.com/openai/codex), or both

Install and authenticate each provider CLI before launching it through Magnus.
Only the CLI you use is required; install both for the default cross-provider
review workflow.

## Installation

Magnus is available from MELPA:

```elisp
(use-package magnus
  :ensure t
  :bind (("C-c m" . magnus)
         ("C-c M" . magnus-create-instance)))
```

Or run `M-x package-install RET magnus RET`.

### From GitHub (Emacs 29+)

Install the latest revision directly:

```text
M-x package-vc-install RET https://github.com/hrishikeshs/magnus RET
```

Or add it to your configuration:

```elisp
(unless (package-installed-p 'magnus)
  (package-vc-install "https://github.com/hrishikeshs/magnus"))
```

See [Getting started](docs/getting-started.md) for `straight.el` and `quelpa`
installation, provider setup, upgrades, and `M-x magnus-doctor`.

## Quick start

1. Run `M-x magnus`.
2. Press `c` for Claude Code, or press `?` and then `X` for Codex.
3. Choose a Git project and work in the native TUI that opens.
4. Return to `*magnus*` and create another agent when the work benefits from a
   second pair of hands.
5. Use `RET` to visit an agent, `m` to steer it, and `a` to handle the next
   agent that needs attention.

Press `?` in `*magnus*` for the complete dispatcher. The minibuffer shows
contextual actions as point moves. In an agent terminal, `C-g` sends Escape to
the provider TUI because Emacs normally reserves `ESC` as a Meta prefix.

## Command reference

The main interactive entry points are:

| Command | Purpose |
|---------|---------|
| `M-x magnus` | Open the status buffer |
| `M-x magnus-create-instance` | Create a Claude Code agent |
| `M-x magnus-create-codex` | Create a Codex agent |
| `M-x magnus-create-headless` | Run a one-off headless Claude task |
| `M-x magnus-context` | Open the current project's shared context |
| `M-x magnus-doctor` | Check dependencies, storage, and runtime health |
| `M-x magnus-upgrade` | Reinstall Magnus with a guarded agent shutdown |

Inside `*magnus*`, press `?` for the contextual command menu. See the full
[command reference](docs/reference.md) for status, trace, context, and review
commands.

## Customization

Magnus uses ordinary Emacs customization variables. For example:

```elisp
;; Provider executables.
(setq magnus-claude-executable "/path/to/claude")
(setq magnus-codex-executable "/path/to/codex")

;; Default project for new agents.
(setq magnus-default-directory "~/workspace")

;; UI and coordination.
(setq magnus-status-show-context-hints t)
(setq magnus-coord-reminder-interval 600)
(setq magnus-attention-auto-approve-patterns nil)

;; Independent review defaults.
(setq magnus-review-default-provider nil) ; use the opposite provider
(setq magnus-review-default-effort 'high)
```

Run `M-x customize-group RET magnus RET` to browse every option. The
[customization reference](docs/reference.md#common-customization) covers
monitoring, traces, background work, review timeouts, and storage.

## Architecture

Magnus is a thin Emacs control plane around the native provider tools:

```text
magnus/
├── magnus.el                          # Entry point and top-level setup
├── magnus-{instances,persistence}.el  # Durable agent identity and state
├── magnus-{process,terminal}.el        # Lifecycle and automated TUI delivery
├── magnus-provider.el                  # Provider registry and dispatch
├── magnus-provider-{claude,codex}.el   # Claude and Codex adapters
├── magnus-{status,transient}.el        # Status buffer and contextual actions
├── magnus-{coord,onboarding}.el        # Shared journal, memory, instructions
├── magnus-{attention,health}.el        # Permission queue and liveness signals
├── magnus-{trace,context}.el           # Provider history and shared notes
├── magnus-{headless,background}.el     # Bounded noninteractive model work
├── magnus-review.el                    # Durable lineages and Git evidence
├── magnus-review-controller.el         # Ephemeral review orchestration
├── magnus-review-ui.el                 # Magit-style review reader
└── magnus-{doctor,environment}.el      # Diagnostics and process environment
```

There is no Magnus daemon or replacement chat runtime. The provider CLIs own
their authentication, network behavior, approvals, and terminal interfaces.
See the [architecture deep dive](docs/architecture.md) for lifecycle,
coordination, persistence, trust boundaries, and review publication.

## Documentation

- [Getting started](docs/getting-started.md) — installation, authentication,
  first agents, Doctor, and provider capabilities
- [Command reference](docs/reference.md) — status keys, traces, attention,
  health, shared context, persistence, and customization
- [Independent reviews](docs/reviews.md) — committed scope, multi-round
  lineages, reader commands, failures, and retries
- [Architecture](docs/architecture.md) — terminals, identity, coordination,
  process ownership, review evidence, and resource bounds
- [Troubleshooting](docs/troubleshooting.md) — common setup and runtime problems
- [Release notes](NEWS.md)

## Development

```sh
make test
make lint
```

## License

MIT
