# Command and customization reference

[Back to README](../README.md) · [Getting started](getting-started.md) ·
[Reviews](reviews.md) · [Troubleshooting](troubleshooting.md)

## Status-buffer commands

These commands are bound directly in `*magnus*`:

| Key | Action |
|-----|--------|
| `RET` | Visit the agent or review at point |
| `c` | Create a Claude Code agent |
| `X` | Create a Codex agent |
| `v` | Request a review, or show actions for the review at point |
| `V` | Show actions for the review at point |
| `k` | Archive an agent |
| `R` | Resurrect an archived agent |
| `r` | Rename an archived agent and migrate its memory home |
| `s` / `S` | Suspend or resume a Claude Code agent |
| `d` | Change project and start a fresh provider session |
| `m` | Send a message to an agent |
| `t` | Open the provider trace |
| `x` | Open shared project context |
| `C` | Open the shared coordination journal |
| `a` / `A` | Visit the next attention request / show the queue |
| `P` | Archive all agents |
| `z` | Toggle Do Not Disturb |
| `F` | Generate a session retrospective |
| `n` / `p` | Navigate agents and reviews |
| `g` | Refresh |
| `?` | Open all commands |
| `q` | Quit |

The `?` dispatcher additionally exposes:

| Key | Action |
|-----|--------|
| `c` | Create a Claude Code agent |
| `h` | Create a headless Claude task |
| `o` | Open the completed review at point |
| `D` | Run `magnus-doctor` |
| `C` | Open `.magnus-coord.md` |
| `I` | Open generated coordination instructions |
| `H` / `T` | Toggle health / attention monitoring |

The minibuffer shows context-sensitive hints as point moves among agents,
reviews, and review rounds. Set `magnus-status-show-context-hints` to nil to
disable them.

## Native agent lifecycle

Each instance has a friendly unique name, a durable UUID, provider session
metadata, and first-person memory at `.claude/agents/NAME/memory.md`.

Archive stops the current process but preserves the identity. Resurrection
returns to the provider session when possible. Rename is intentionally
available only for archived agents; Magnus moves the memory home
transactionally while the UUID and review history remain unchanged.

Changing an agent's directory starts a fresh provider session. The identity
remains the same, but Magnus does not pretend that a session rooted in one
project can safely continue in another.

With current Claude CLIs, Magnus assigns an exact `--session-id` to a fresh
terminal. On older CLIs it falls back to detecting the one new local session
and permits only one unresolved fresh Claude launch per physical project in
that Emacs session.

## Traces

Press `t` on an agent to read the provider's local session record.

- Claude traces include recorded thinking blocks and responses.
- Codex traces include visible engineering journals, user messages, and
  responses. Encrypted internal reasoning is not available.

| Key | Trace action |
|-----|--------------|
| `TAB` | Fold the thinking block at point |
| `t` | Toggle all thinking blocks |
| `n` / `p` | Move between responses |
| `[` | Load earlier history |
| `g` | Refresh |
| `G` | Jump to the end |
| `q` | Close |

Disk reads and rendering are bounded independently by
`magnus-trace-read-chunk-bytes`, `magnus-trace-max-record-bytes`, the initial
entry limit, and the rendered line limit.

## Attention and health

Magnus detects approval and confirmation prompts and places agents in an
attention queue. Safe patterns may be auto-approved. Set
`magnus-attention-auto-approve-patterns` to nil to disable auto-approval.

Health indicators summarize terminal activity:

- `+` — active and changing;
- `~` — stale;
- `!` — stuck across consecutive checks;
- `x` — no live buffer or process.

## Shared context and user-facing headless tasks

Press `x` for a persistent per-project scratch buffer. It can hold notes and
links, fetch and cache URL content, export to `.magnus-context.md`, or copy its
contents for an agent.

| Key | Context action |
|-----|----------------|
| `C-c C-u` | Insert and fetch a URL |
| `C-c C-f` | Fetch the URL at point |
| `C-c C-e` | Export project context |
| `C-c C-c` | Copy context |
| `C-c C-s` | Save |

`M-x magnus-create-headless` (or `? h`) runs a user-invoked fire-and-forget Claude
task without a `vterm`. Its output appears in a read-only buffer and the
instance ends as `finished`, `errored`, or `stopped`. This user-facing helper is
separate from the provider-neutral headless runner used internally by reviews.

Low-priority model work—expertise indexing, retrospectives, and optional
dashboard messages—shares one FIFO with one active provider process. Queue,
output, runtime, memory input, and synchronous matching all have configurable
bounds.

## Durable storage

Magnus keeps user state under `~/.magnus/` by default:

| Path | Contents |
|------|----------|
| `~/.magnus/state.el` | Agent registry and provider session metadata |
| `~/.magnus/reviews/` | Review manifests, rounds, evidence, and reports |
| `~/.magnus/context/` | Per-project shared context |
| `~/.magnus/url-cache/` | Fetched context URLs |
| `~/.magnus/agents-index.el` | Dormant-agent expertise index |
| `~/.magnus/attention.el` | Learned attention data |

Active projects may also contain `.magnus-coord.md`, generated instructions at
`.claude/magnus-instructions.md`, and per-agent memory below
`.claude/agents/`.

## Common customization

```elisp
;; Provider commands.
(setq magnus-claude-executable "/path/to/claude")
(setq magnus-codex-executable "/path/to/codex")

;; Default project for new agents.
(setq magnus-default-directory "~/workspace")

;; Independent review defaults.
(setq magnus-review-default-provider nil) ; opposite the author
(setq magnus-review-default-effort 'high)
(setq magnus-review-directory-root "~/.magnus/reviews")
(setq magnus-review-delivery-timeout 180)
(setq magnus-review-scope-timeout 180)
(setq magnus-review-timeout 3600)

;; Shared low-priority model work.
(setq magnus-background-queue-limit 32)
(setq magnus-background-output-limit (* 256 1024))
(setq magnus-background-timeout 90)
(setq magnus-agents-index-memory-limit (* 64 1024))
(setq magnus-expertise-match-timeout 15)
(setq magnus-health-dashboard-ai-messages nil)

;; UI and monitoring.
(setq magnus-status-show-context-hints t)
(setq magnus-status-review-animation-interval 0.4)
(setq magnus-attention-auto-approve-patterns nil)
(setq magnus-health-check-interval 30)
(setq magnus-health-stale-threshold 120)
(setq magnus-health-stuck-threshold 3)

;; Bounded provider traces.
(setq magnus-trace-read-chunk-bytes (* 64 1024))
(setq magnus-trace-max-record-bytes (* 1024 1024))

;; Coordination housekeeping and reminders.
(setq magnus-coord-log-max-entries 25)
(setq magnus-coord-reminder-interval 600)
```

Use `M-x customize-group RET magnus RET` for every option.
