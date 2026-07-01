# Serenity Coding Standards

These standards describe how Serenity code should be shaped as the project grows.
They are intentionally short; use them to guide design choices, issue breakdowns,
and code review.

## Functional Purity

Keep domain transformations pure. Reducers, layout calculators, text model
operations, schema derivation, and view-model builders should be ordinary
functions over immutable values whenever they can be.

Use `IO` at real effect boundaries: file IO, Swing interaction, logging, timers,
clipboard access, LSP processes, queues, refs, and application lifecycle wiring.
Do not wrap pure work in `IO(...)` unless the work is deliberately delayed or
expensive enough that delaying it is part of the contract.

Represent parse, validation, and recovery decisions explicitly with `Either`,
`Option`, or domain types. Avoid silent fallback spread across call sites; a
fallback policy should be named and tested where it is introduced.

## Mutation Policy

Public and domain APIs should behave as immutable transformations. Prefer
`case class` copies, immutable collections, and value-returning helper methods.

Private local mutation is acceptable when it is contained and earns its place,
for example `StringBuilder` during rendering or rope traversal. It must not leak
through public APIs, captured closures, or shared state.

Shared state belongs behind Cats Effect primitives such as `Ref`, `Deferred`,
`Queue`, or streams. Avoid `var`, mutable collections, and ad hoc synchronization
for cross-component state.

## Module Boundaries

Keep pure reducers and calculators separate from interpreters that perform IO.
Runtime loops should orchestrate dependencies; they should not own business
rules that could live in a pure function.

Prefer small capability traits, focused data models, and explicit dependency
bundles over god traits, god objects, and large methods that mix UI, persistence,
schema, and state-machine responsibilities.

Keep UI schema descriptions separate from runtime state machines. Command
surface definitions, config schema metadata, and rendering view models should
describe structure; reducers and interpreters should perform behavior.

## Configuration And Schema Ownership

Configuration fields should have one source of truth for their type, default,
description, migration behavior, and user-facing surface where practical.

When adding or changing config, update the parser, renderer, migration warnings,
session codecs, preset patches, and command-runner settings from the same
schema concept. If full derivation is not yet possible, keep duplication local
and covered by tests.

Schema changes must preserve existing user data. Add migrations or explicit
warnings for renamed, removed, or invalid fields.

## Testing Standards

Use TDD where possible: write or update the behavior/regression test before the
implementation. Tests should describe the contract, not implementation trivia.

Normal test suites should avoid trace output and debug-era names. Keep
diagnostic output in dedicated benchmark or tooling entry points, not ordinary
ScalaTest runs.

Core data structures and indexing rules need representation-invariance coverage.
For example, rope behavior should be tested independently of tree shape, and
text indexing changes should assert both visible text behavior and internal
UTF-16 cursor or range positions.

Run focused tests for the changed behavior, then run formatting and Scalafix for
Scala changes. Use the checks in `DEVELOPMENT.md` before opening or updating a
PR.

## Issue Guidance

Use these standards to split and review architecture work such as #310, #311,
#319, #376, and #377. Broad issues should be reduced into PR-sized changes that
move one boundary, schema owner, or runtime responsibility at a time.
