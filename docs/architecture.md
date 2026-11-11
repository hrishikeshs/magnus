# Architecture and trust boundaries

[Back to README](../README.md) · [Getting started](getting-started.md) ·
[Reviews](reviews.md) · [Reference](reference.md)

Magnus is deliberately a thin Emacs control plane around provider tools, not a
replacement runtime.

## Native where it matters

Claude Code and Codex run directly in `vterm`. Magnus does not emulate their
interfaces and does not connect a second client to an app server. Each
provider's approval flow, command composer, slash commands, authentication,
streaming, and terminal behavior remain owned by that provider.

Magnus adds the layer a terminal does not have:

- durable identity and provider-session metadata;
- lifecycle transactions and rollback;
- status, attention, health, and trace presentation;
- shared project coordination and memory;
- exact-evidence independent review.

There is no Magnus daemon or hosted Magnus service. Provider CLIs retain their
normal network behavior. No Magnus-specific plugin, skill, or provider app
server is required.

## Identity and lifecycle

Instances use a durable UUID as identity and a unique friendly name for humans,
terminal buffers, memory homes, and legacy routing. Runtime ownership is
first-writer-wins: a stale process, timer, sentinel, or provider callback must
prove it still owns the exact instance before mutating state.

Creation, project moves, archive, and resurrection acquire resources in an
explicit order and roll back only what the failing operation acquired. A
replacement process or buffer cannot be overwritten by a delayed callback from
its predecessor.

Headless tasks have a durable kind distinct from interactive agents. A restored
headless record never becomes an interactive terminal by accident.

## Terminal transport

Magnus treats a selected interactive TUI as human-owned and defers automated
paste. All Magnus-delivered submissions share one scoped per-process FIFO
rather than racing independent calls into `vterm`. Paste and Return are owned
as one logical submission; if Return fails after a successful paste, Magnus
retries only Return and fences the pending operation to the same process.

That substrate is also available to bridge integrations, which can ask whether
an immediate submission was accepted without opening a second transport path.

## Coordination

Agents coordinate through `.magnus-coord.md`, an ordinary Markdown project
journal with four sections:

- **Active Work** — ownership, status, and files;
- **Discoveries** — project facts and gotchas;
- **Decisions** — choices that should outlive a conversation;
- **Log** — announcements, mentions, and direct messages.

The Log is stored newest first. Magnus normalizes it to chronological order for
status and retrospective readers, watches it while a project is active, and
delivers addressed messages through the agent's ordinary terminal transport.

Provider-neutral instructions are generated at
`.claude/magnus-instructions.md` and included in Magnus onboarding. The name of
the directory is historical; both Claude and Codex receive the same protocol.

Coordination housekeeping and reminders are bounded and may be disabled.

## Headless execution

Reviews use a shared provider-neutral headless runner but own their individual
processes directly, so independent reviews may execute concurrently.

Optional low-priority work—expertise indexing, retrospectives, and generated
dashboard messages—uses a separate FIFO with one active process. Queue length,
retained output, input size, record size, and runtime all have explicit bounds.

Provider decoders consume framed JSONL incrementally, isolate malformed lines,
and require purpose-specific success evidence. A review cannot weaken the
structured-result requirements declared by its provider adapter.

## Completed review lineages

The review controller owns ephemeral scope discovery and process execution.
The durable review model owns only successful lineages, immutable Git evidence,
finding identity, and publication. The reader owns presentation and never
mutates lineage state.

Candidates run in private detached checkouts. Publication verifies exact
evidence digests, result schema, anchors, revision freshness, and the
one-open-lineage-per-project/author invariant. Native Emacs file locks protect
publication across processes and recover locks whose owning process died.

See [Independent reviews](reviews.md) for the user-facing lifecycle.

## Persistence and project files

The agent registry and provider-session metadata live in
`~/.magnus/state.el`. Reviews, context, URL cache, expertise data, and attention
data have separate paths below `~/.magnus/`.

Instance state and review storage have the strongest boundary checks: Magnus
validates records before replacing live state, rejects unsafe identities and
path escapes, refuses symlinked storage boundaries, tightens private
permissions, and publishes replacements atomically. The context, cache,
expertise, and attention stores use their own simpler persistence paths and do
not claim that complete hardening model.

The project-visible files are intentionally readable text:

- `.magnus-coord.md` for shared coordination;
- `.claude/magnus-instructions.md` for the generated protocol;
- `.claude/agents/NAME/memory.md` for first-person agent memory;
- optional `.magnus-context.md` when a user exports shared context.

Review artifacts live outside the repository because they follow fast-moving
commits and are not project source.

## Shutdown and restart

Magnus teardown is idempotent. It cancels its timers, watchers, terminal
deliveries, background work, and review processes, then persists owned state.
One subsystem's teardown failure does not prevent the others from releasing
their resources.

Unfinished reviews do not survive an Emacs restart. Completed rounds do. After
upgrading Magnus, restart Emacs rather than mixing loaded definitions from two
versions.
