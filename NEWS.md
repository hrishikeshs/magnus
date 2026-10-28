# Magnus release notes

## 0.2.0

Magnus 0.2.0 turns the original Claude-oriented instance manager into a
provider-neutral, resource-bounded agent workspace.

### Highlights

- Run Claude Code and Codex side by side in their native `vterm` TUIs, with
  shared durable identity, onboarding, memory, and lifecycle behavior.
- Request headless reviews from either provider over exact committed Git
  ranges. Successful rounds open in a folding Magit-style diff, and the same
  reviewer identity and provider session continue across follow-up rounds.
  Every successful result remains in the canonical finding lineage; failed
  session resumption can retry the retained candidate in a fresh session.
- Coordinate agents through the shared `.magnus-coord.md` project journal.
  Reviews use the author's ordinary provider conversation instead of adding a
  second protocol to that journal.
- Diagnose provider CLIs, libraries, Git, durable storage, and coordination
  state with `M-x magnus-doctor`.

### Behavioral changes

- Renaming is now archived-only and transactional: Magnus moves the agent's
  memory home together with its unique registry name or rolls both back.
  Project moves and resurrection likewise roll registry and process state back
  if provider startup fails.
- Fresh Claude sessions use an exact `--session-id` on current CLIs. Older CLIs
  retain unique-delta discovery, but fresh launches are serialized per physical
  project within an Emacs session until that legacy capture resolves.
- Startup does not replay old conversational chatter or unfinished review
  execution. Completed review lineages remain available; pending author
  queries and reviewer processes are intentionally disposable. A manually
  terminated headless task is `stopped`, while a normal nonzero process exit is
  `errored`.
- `C` opens the shared coordination journal, and manual `g` polls watched
  projects for ordinary coordination messages.
- Away/back lifecycle notices are coalesced per agent until its terminal can
  accept them; contradictory undelivered transitions disappear without
  affecting append-only peer messages.
- AI-generated dashboard fortunes are now opt-in; static messages remain the
  default.
- `magnus-onboarding-user-instructions` can replace only the user-facing
  response guidance in initial onboarding while Magnus retains generated
  identity, memory, coordination, summon, and authorization context.

### Resource hardening

- Independent reviews own their headless provider processes directly, may run
  concurrently, and time out abandoned provider runs independently.
- Expertise indexing, retrospectives, and optional dashboard generations share
  one low-priority FIFO with a single active process and bounded queue, output,
  and timeout settings; expertise indexing also bounds memory input.
- Headless provider parsing bounds stderr, JSONL records, and retained errors;
  trace viewing uses bounded disk chunks, records, initial entries, and buffer
  lines.
- Coordination-log housekeeping remains bounded.
- CI covers Emacs 28.1, 29.4, and 30.2.

The coordination protocol is supplied through ordinary onboarding and
`.claude/magnus-instructions.md`; no Magnus plugin or agent skill is required.
