# Development

## Active Codespace

Use one persistent Codespace as the cloud development machine, then create normal PR branches from inside it.

Suggested setup:

```bash
git switch master
git pull
git switch -c codex/serenity-active
git push -u origin codex/serenity-active
```

Create the Codespace from `codex/serenity-active` and keep reopening that same Codespace from `https://github.com/codespaces`.

For a specific change, branch from an up-to-date `master` inside the Codespace:

```bash
git switch master
git pull
git switch -c codex/my-change
```

## Checks

Project coding standards live in `docs/coding-standards.md`. Read that document
before changing core reducers, runtime loops, configuration schema, rendering,
or state-management code.

Run the Scala checks before opening or updating a PR:

```bash
sbt -v test assembly architectureCheck
```

Run formatting and Scalafix before committing when changing Scala code:

```bash
sbt -v scalafmtAll "Compile / scalafix" "Test / scalafix"
```

## Automated standards

Three layers enforce `docs/coding-standards.md` rather than leaving it to review.

**Scalafix** (`.scalafix.conf`) bans `var`, `return`, `while`, and `final val`, and organises imports.
Local mutation that genuinely earns its place is scoped with `// scalafix:off DisableSyntax`, as in
`LspFramer`.

**WartRemover** (configured in `build.sbt`, main sources only) adds rules the compiler cannot express:

| Enforced as errors | What it protects |
| --- | --- |
| `FinalCaseClass`, `LeakingSealed` | Data types stay closed; nobody re-opens a sealed hierarchy |
| `TripleQuestionMark`, `ThreadSleep` | The "no stubs" and "no blocking in IO" rules, held at zero |
| `AsInstanceOf`, `IsInstanceOf` | Pattern matching over casting |

`Null`, `Throw`, `OptionPartial` and `IterableOps` are reported as **warnings only** — each needs its
own migration. Java/AWT interop (`ui/terminal`, `ui/accessibility`), the LSP framer and the richtext
codecs are excluded, since they sit at the outermost boundary where those constructs are legitimate.
Tests are exempt entirely.

To take a deliberate exception, annotate the declaration and say why:

```scala
@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
case class Leaf(...)   // subclassed by RopeSpec to prove search never materialises the rope
```

**`architectureCheck`** enforces size and layering against `project/architecture-baseline.tsv`:
methods ≤ 80 lines, files ≤ 600 lines, and no `java.awt`/layout-engine imports inside
`state/reducers`. It is a ratchet, not a threshold — the baseline lists what is already over, and the
build fails if a file gets worse, a new one starts over target, or a layer is crossed. Fixing an
entry means deleting it:

```bash
sbt writeArchitectureBaseline   # after removing a violation, to bank the win
```

The baseline may shrink, never grow. If you must add to it, explain the entry in review.

## Codex CLI

Codex CLI is installed in the Codespace image. Start it from the repository root:

```bash
codex
```

Sign in with ChatGPT when prompted, or add `OPENAI_API_KEY` as a Codespaces secret before creating or rebuilding the Codespace if you prefer API-key auth.

Desktop package checks run in GitHub Actions on PRs. Release publishing is handled by the dedicated desktop publish workflow on `master` or manual dispatch.
