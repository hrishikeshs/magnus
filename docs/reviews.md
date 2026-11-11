# Independent reviews

[Back to README](../README.md) · [Getting started](getting-started.md) ·
[Reference](reference.md) · [Architecture](architecture.md)

Magnus turns “open another model and ask it to review this” into a repeatable,
headless workflow. The programmer selects an author; Magnus discovers the
author's committed scope, runs a named reviewer over immutable evidence, and
publishes a structured result only after successful validation.

## Request a review

In `*magnus*`:

1. Make sure the author's project is a Git repository.
2. Ask the author to commit the completed work.
3. Leave the worktree clean. Magnus ignores its configured coordination
   journal and generated instructions, but other tracked or untracked work
   blocks the request.
4. Put point on the author and press `v RET`.

The request popup also accepts optional overrides:

| Key | Option |
|-----|--------|
| `p` | Provider (`opposite`, `claude`, or `codex`) |
| `m` | Provider model |
| `e` | Reasoning effort |
| `RET` | Start the request |

By default the reviewer uses the provider opposite the author. Magnus reuses
its existing expertise matching when it chooses a reviewer identity.

## Scope discovery

Magnus does not guess a commit range. It asks the author, through the author's
ordinary terminal conversation, which exact committed `base..head` range
represents the work.

The reply is correlated to the exact provider session and a request nonce.
Magnus accepts only canonical full object IDs, proves that the base is an
ancestor of the head, and proves that the head remains reachable from the
current branch. A moved branch, changed answer, dirty worktree, or unrelated
task blocks the request instead of silently changing the evidence.

Queued terminal delivery and the response are bounded by
`magnus-review-delivery-timeout` and `magnus-review-scope-timeout`.

## Reviewer execution

Each review candidate owns a headless provider process directly. Independent
reviews can run concurrently with one another and with the low-priority
background queue used for indexing and retrospectives.

The reviewer runs inside a private detached checkout derived from the immutable
round number and head commit. Rounds never share a mutable worktree. Magnus
rejects tracked, untracked, or ignored residue in one of its managed checkouts.

The prompt contains:

- the exact committed scope and changed paths;
- the patch and name-status evidence;
- project and task context;
- the successful prior review lineage, when this is a later round;
- a strict structured-result contract with line-addressed findings.

The provider run is bounded by `magnus-review-timeout`.

## Durable boundary

Pending review work is deliberately disposable. The author query, candidate
checkout, provider process, failures, and retries live only in the current
Emacs session. Restarting Emacs discards unfinished execution instead of
replaying stale conversational state.

Only a successful, validated round becomes durable. Magnus stores under
`~/.magnus/reviews/`:

- the exact base and head commits;
- patch and changed-path evidence with SHA-256 digests;
- the structured result and Markdown report;
- verdict, finding count, and read state;
- reviewer identity and last successful provider session;
- a monotonic manifest revision.

Publication uses Emacs's native cross-process file locks. First publication
also locks the project/author identity, so two Emacs processes cannot create
parallel open lineages for the same author's work. Dead-owner locks are
reclaimed by the native lock protocol; a live owner is refused.

## Multi-round review

After the author addresses findings and commits another clean range, request
the next round from the existing review's action menu.

Magnus keeps the same reviewer name and resumes its successful provider session
when possible. Every historical finding ID remains reserved. Findings from the
immediately preceding round require dispositions; a resolved older finding may
retain its identity if it resurfaces later.

Missing or corrupt prior evidence blocks the next round rather than silently
starting a disconnected review.

When that body of work is finished, archive the review lineage. Archived
lineages are hidden from the working view by default; press `U` in `*magnus*`
to reveal them. Their reports and immutable evidence remain available. A
future task gets a new reviewer lineage even if expertise matching chooses the
same named specialist.

## Review reader

Completed reviews appear in a separate status section with an unread marker.
Press `RET` on a review or completed round to open its Magit-style reader.

| Key | Action |
|-----|--------|
| `TAB` | Fold or unfold a file, hunk, or finding |
| `n` / `p` | Next or previous visible section |
| `N` / `P` | Next or previous finding |
| `RET` | Open the file from the reviewed Git snapshot |
| `e` | Open the corresponding current worktree file |
| `[` / `]` | Previous or next review round |
| `?` | Open review actions |
| `g` | Refresh from the latest durable manifest revision |
| `q` | Close the reader |

The reader cross-checks the structured result against immutable evidence.
Unsafe paths, mismatched files, invalid anchors, incomplete Git type changes,
or a corrupt result keep the round unreadable rather than presenting a
plausible but false review.

## Failure, interruption, and retry

The review action menu reflects the current ephemeral state:

- interrupt an author query or running reviewer;
- retry failed or interrupted work during the same Emacs session;
- retry a retained candidate with the same reviewer identity and exact
  evidence in a fresh provider session;
- request the next committed round after a successful result;
- archive the lineage when the task is complete.

Failures never enter the durable lineage. If Emacs restarts, select the author
or existing completed review and request the work again from current committed
evidence.

## Status and notifications

While a reviewer is executing, the author row shows an animated review badge
(or a static badge when animation is disabled). On success, Magnus refreshes
the review section and tells the author where the findings are available. If
the author is not currently loaded, the completed review remains available in
`*magnus*` without inventing a delivery path.
