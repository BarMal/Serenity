# Claude Global Instructions

## How to Approach Work

**Exception — skip Phase 1** if the change touches a single file that has already been read in the current session and the scope is clearly small (a one-liner, a rename, a trivial fix). Go straight to Phase 2 in that case. If in doubt, do Phase 1.

### Phase 1 — Explore & Learn

Before proposing anything, do the reading. For any non-trivial task:

1. Read all relevant source files, tests, configuration, and documentation
2. Build a complete picture of the problem — what exists, how it fits together, what constraints apply
3. Present your findings with **academic-style references** — every claim backed by a numbered citation to the exact source:
   - Code: `[1] src/main/scala/com/serenity/state/models/AppState.scala`
   - Docs: `[2] https://typelevel.org/cats-effect/docs/std/ref`
   - sbt output, config files, etc. are all citable sources
4. Do not propose solutions during Phase 1. The goal is understanding, not answers

### Phase 2 — Propose & Discuss

Only after Phase 1 is complete, propose a solution. Structure it as a numbered list of discrete changes, each described clearly enough to discuss individually. Wait for sign-off on the full list before touching any code.

Iterate on the proposal via discussion until it's agreed. Then implement one change at a time, re-reading the current state of affected files between each change to catch any drift or side-effects before proceeding to the next.

---

**Clarify ambiguity upfront.** If a request is underspecified, ask one focused question to resolve the key ambiguity before starting Phase 1. Don't ask multiple questions or mid-task questions unless something genuinely unexpected comes up.

**Prefer surgical changes.** Make the smallest change that solves the problem. Don't refactor unrelated code, rename things for style, or add abstractions that weren't requested. If you notice something worth fixing while working, mention it separately rather than silently changing it.

**Never delete files or make destructive changes without explicit instruction.**

**Be honest, not agreeable.** If a plan has a flaw, if an approach is wrong, or if something won't work — say so directly. Don't validate ideas just to avoid friction. Pushback that saves time later is more useful than agreement that feels good now.

**Be direct.** The goal is effective work, not a pleasant interaction. Drop filler phrases like "Great question!", "Certainly!", "Absolutely!", and "Happy to help!" — they add nothing. Don't compliment ideas before critiquing them. Don't soften a "no" into a "yes, but". Say what you mean, then do it. Hollow positivity before bad news is worse than useless — it delays the actual information and erodes trust when the work falls short.

**Signal uncertainty explicitly.** When you are uncertain, or when a statement is based on inference rather than concrete code or documentation, preface it with:

> Caution, here be imagine dragons:

This applies to: guesses about runtime behaviour, inferences from incomplete reads, assumptions about library internals not confirmed by docs, and any claim where you lack a citable source. Never present speculation as fact.

---

## Scala / Functional Programming Style

### Language & Tooling
- **Scala 3** — use Scala 3 syntax throughout (indentation-based blocks, `enum`, `given`/`using`, extension methods, etc.). No Scala 2 brace-heavy style.
- **Build tool**: sbt
- **Effect system**: Cats Effect (`IO`, `Resource`, `Ref`, `Deferred`) — always prefer `IO` over raw `Future` or `Try`
- **Streaming**: FS2 (`Stream[IO, A]`) for any streaming or iterative data pipelines
- **Functional core, effectful shell**: keep pure logic in plain functions/case classes; push `IO` to the edges

### Code Style
- Prefer `case class` and `sealed trait`/`enum` for data modelling; use `opaque type` for newtypes
- Use `Option`, `Either[E, A]`, or `IO[A]` for error handling — no exceptions except at the outermost boundary
- Keep functions small and single-purpose; prefer composition over inheritance
- Companion objects for smart constructors and `apply`/`empty` helpers
- Meaningful names that reflect the domain, not the implementation (`AnimationState`, not `StateData`)
- Doc comments (`/** ... */`) on public API methods and types; inline comments only when the *why* isn't obvious from the code
- Keep imports explicit — no wildcard imports except for `cats.syntax.all._` or `cats.implicits._`
- Run `scalafix` for unused import cleanup (semanticdb is enabled)

### What to Avoid
- `var`, mutable collections, or shared mutable state — use `Ref[IO, A]` instead
- `null` — use `Option`
- Partial functions (`head`, `get`) on collections or `Option` without safety checks
- `Thread.sleep` or blocking calls inside `IO` — use `IO.sleep` and `IO.blocking` appropriately
- `throw` inside `IO` computations — use `IO.raiseError` or `EitherT`

---

## Workflow & Process

### Before Starting
- Read the relevant existing code before proposing changes — don't guess at types, names, or structure
- If touching a module for the first time, quickly scan the package to understand the conventions in use there

### Testing — TDD is non-negotiable

This project follows TDD. Tests define the contract; the implementation serves the tests, never the other way around.

- Write or update tests **before** writing implementation code
- A task is only complete when all tests pass — not when the code compiles, not when it "looks right"
- **Never delete, comment out, skip (`ignore`/`pending`), or otherwise disable a failing test.** A failing test is a signal, not an obstacle. If a test fails, fix the code until it passes. The only exception is if the test itself is provably wrong — and even then, ask before removing it
- **Never claim a feature is complete if any tests are failing.** Report the failure and what's needed to fix it
- Do not write placeholder test bodies (`???`, `pending`, `// TODO`) and present them as coverage

### No Stubs, No Deferrals

Deliver complete implementations. Do not leave work half-done and present it as done.

- **No `TODO`, `FIXME`, or `HACK` comments in new code.** If something genuinely needs a follow-up, raise it explicitly in your response so it can be tracked — don't bury it in the code
- **No stub implementations** — no `???`, no `throw new NotImplementedError(...)`, no methods that return dummy/empty values where real logic is expected
- If you can't implement something fully in one step, say so upfront and agree on scope before starting. Delivering a partial implementation without flagging it is worse than not starting

### Running sbt

This machine can be slow. **Never add a timeout to an sbt command.** If a build or test run is taking a long time, let it run to completion — do not kill it, retry with a shorter timeout, or assume it has hung. Wait.

- Run `sbt` commands without timeout flags
- If a command appears to hang for an unusually long time, report it and ask — don't cancel and retry on your own

### Commits & Changes
- Write clear, imperative commit messages: `Add fade-in animation for new buffers`, not `changes`
- Group related changes into one commit; don't bundle unrelated fixes

### Communicating Progress
- When a task spans multiple steps, give a brief status after each meaningful step
- If you hit an unexpected blocker, say so immediately rather than working around it silently
- Summarise what you changed at the end — file names, what was added/modified/removed
