# magnus

Magnus is a magit-like interface built within Emacs for managing multiple Claude Code instances.

## Features

- **Instance management**: Create, kill, rename, and switch between Claude Code instances
- **Status buffer**: See all instances at a glance with their status and working directory
- **Agent coordination**: Agents communicate through a shared coordination file to avoid conflicts
- **Shared context**: Per-project scratch buffer for notes and links that all agents can access
- **URL fetching**: Paste URLs and magnus will fetch and cache the content
- **Persistence**: State persists across Emacs sessions

## Requirements

- Emacs 28.1+
- [vterm](https://github.com/akermu/emacs-libvterm)
- [transient](https://github.com/magit/transient) (built into Emacs 28+)

## Installation

```elisp
(add-to-list 'load-path "/path/to/magnus")
(require 'magnus)
```

## Usage

Run `M-x magnus` to open the status buffer.

### Key bindings (status buffer)

| Key   | Action                    |
|-------|---------------------------|
| `c`   | Create new instance       |
| `RET` | Switch to instance        |
| `k`   | Kill instance             |
| `K`   | Force kill instance       |
| `r`   | Rename instance           |
| `R`   | Restart instance          |
| `x`   | Open context buffer       |
| `C`   | Open coordination file    |
| `g`   | Refresh                   |
| `?`   | Show help menu            |
| `q`   | Quit                      |

### Context buffer

The context buffer (`M-x magnus-context` or `x` in status buffer) is a per-project scratch pad for sharing notes, links, and context with your Claude Code instances.

| Key       | Action                     |
|-----------|----------------------------|
| `C-c C-u` | Insert and fetch URL       |
| `C-c C-f` | Fetch URL at point         |
| `C-c C-e` | Export to temp file        |
| `C-c C-c` | Copy to clipboard          |

Content persists across sessions but is stored in `~/.emacs.d/magnus-context/`, not in your project.

### Agent coordination

When you create instances, magnus sets up a coordination system so agents can communicate:

- **`.magnus-coord.md`**: Shared file where agents announce their work and communicate
- **`.claude/magnus-instructions.md`**: Instructions for agents on how to use the coordination protocol

Agents are instructed to:
1. Check the coordination file before starting work
2. Announce what files they're touching in the "Active Work" table
3. Communicate with other agents in the "Log" section
4. Record decisions that affect others

This prevents agents from stepping on each other's work. The magnus status buffer shows active work and recent coordination messages.

Example coordination flow:
```
[10:30] swift-fox: Starting work on auth module. Will touch src/auth/*.ts
[10:31] keen-owl: Noted. I'll work on the API tests instead.
[10:45] swift-fox: Auth done. @keen-owl you can proceed with auth tests.
```
