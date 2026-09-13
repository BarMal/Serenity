package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for issue #1454: `CommandRunner.findGroup`/`groupPaths` recurse into `group.children` with no depth/
  * seen-set guard. Not currently reachable in production -- `settingsGroups` is generated from static definitions,
  * presumably acyclic by construction -- but nothing enforces that invariant, so a future settings-group addition
  * that accidentally nests a group under itself would previously stack-overflow rather than fail gracefully.
  *
  * These specs exercise `findGroup`/`groupPaths` directly (both `private[command]`, exactly so this suite can reach
  * them without routing a synthetic group tree through all of `CommandRunner`'s public surface) against a
  * hand-built self-nested `GroupItem` -- a group of id `"self"` whose sole child is again `"self"`, repeated to a
  * depth that would overflow the JVM's default stack under the old unguarded recursion.
  */
class CommandRunnerGroupTraversalGuardSpec extends AnyFlatSpec with Matchers:

  private val runner = CommandRunner.empty

  /** A group literally nested under itself `depth` times -- id `"self"` at every level, terminating in a `Nil`-
    * children leaf still labelled `"self"`. This is exactly the "accidentally nests a group under itself" shape
    * the issue describes: a real (not merely deep) self-reference by id, built as a finite immutable tree since
    * `GroupItem.children` is a strict `List`.
    */
  private def selfNestedGroup(depth: Int): CommandSurfaceItem.GroupItem =
    val leaf = CommandSurfaceItem.GroupItem("self", "Self", Nil, CommandCategory.Settings)
    // Built with `foldLeft` (an iterative loop under the hood), not naive recursion -- a depth in the hundreds of
    // thousands would itself overflow the stack while *constructing* the fixture otherwise, before the traversal
    // under test ever runs.
    (1 to depth).foldLeft(leaf) { (child, _) =>
      CommandSurfaceItem.GroupItem("self", "Self", List(child), CommandCategory.Settings)
    }

  /** Large enough that the old unguarded `findGroup`/`groupPaths` (each level pays several stack frames across
    * `collectFirst`/`orElse`/`flatMap`/`view`/`flatMap`) would exhaust the default JVM thread stack; small enough
    * that a guarded traversal -- which stops recursing into an already-visited group id -- returns effectively
    * immediately regardless of depth.
    */
  private val overflowingDepth = 500_000

  /** A genuine two-node cycle -- id `"a"` nested under id `"b"` nested under id `"a"`, alternating to `depth`
    * levels -- rather than a single group nested under itself. Reproduces the guard against the exact shape the
    * review called out: "a longer cycle A -> B -> A", not just a self-nested single id.
    */
  private def alternatingCycle(depth: Int): CommandSurfaceItem.GroupItem =
    val leaf = CommandSurfaceItem.GroupItem("a", "A", Nil, CommandCategory.Settings)
    (1 to depth).foldLeft(leaf) { (child, level) =>
      val id = if level % 2 == 0 then "a" else "b"
      CommandSurfaceItem.GroupItem(id, id.toUpperCase, List(child), CommandCategory.Settings)
    }

  "findGroup" should "not stack-overflow on a group self-nested to overflowing depth" in {
    val cyclic = List(selfNestedGroup(overflowingDepth))

    noException should be thrownBy runner.findGroup("does-not-exist", cyclic)
  }

  it should "still find a self-nested group's own id without recursing past the first occurrence" in {
    val cyclic = List(selfNestedGroup(overflowingDepth))

    runner.findGroup("self", cyclic).map(_.id) shouldBe Some("self")
  }

  it should "find a group nested a normal (non-pathological) number of levels deep" in {
    val leaf   = CommandSurfaceItem.GroupItem("leaf", "Leaf", Nil, CommandCategory.Settings)
    val middle = CommandSurfaceItem.GroupItem("middle", "Middle", List(leaf), CommandCategory.Settings)
    val root   = CommandSurfaceItem.GroupItem("root", "Root", List(middle), CommandCategory.Settings)

    runner.findGroup("leaf", List(root)) shouldBe Some(leaf)
    runner.findGroup("middle", List(root)) shouldBe Some(middle)
    runner.findGroup("missing", List(root)) shouldBe None
  }

  it should "not stack-overflow on a genuine multi-node cycle (A -> B -> A -> ...)" in {
    val cyclic = List(alternatingCycle(overflowingDepth))

    noException should be thrownBy runner.findGroup("does-not-exist", cyclic)
  }

  it should "not falsely prune an unrelated top-level sibling's id when descending into a different group's children" in {
    // Regression for the review finding on PR #1500: `nextVisited` must only ever carry ids on the *current*
    // ancestor path. Seeding it from every sibling at a level (rather than just the group being descended into)
    // would make `dup` (nested under `group-a`) look like an already-visited id purely because `group-b` -- an
    // unrelated sibling -- happens to share that id, silently pruning `target` as a false "cycle".
    val target = CommandSurfaceItem.GroupItem("target", "Target", Nil, CommandCategory.Settings)
    val dup    = CommandSurfaceItem.GroupItem("dup", "Dup", List(target), CommandCategory.Settings)
    val groupA = CommandSurfaceItem.GroupItem("group-a", "Group A", List(dup), CommandCategory.Settings)
    val groupB = CommandSurfaceItem.GroupItem("dup", "Unrelated sibling sharing id 'dup'", Nil, CommandCategory.Settings)

    runner.findGroup("target", List(groupA, groupB)) shouldBe Some(target)
  }

  "groupPaths" should "not stack-overflow on a group self-nested to overflowing depth" in {
    val cyclic = List(selfNestedGroup(overflowingDepth))

    noException should be thrownBy runner.groupPaths("does-not-exist", cyclic)
  }

  it should "report exactly one path to a self-nested group's id, not one per repetition" in {
    val cyclic = List(selfNestedGroup(overflowingDepth))

    runner.groupPaths("self", cyclic) shouldBe List(List("self"))
  }

  it should "report every path to a group nested a normal (non-pathological) number of levels deep" in {
    val leaf   = CommandSurfaceItem.GroupItem("leaf", "Leaf", Nil, CommandCategory.Settings)
    val middle = CommandSurfaceItem.GroupItem("middle", "Middle", List(leaf), CommandCategory.Settings)
    val root   = CommandSurfaceItem.GroupItem("root", "Root", List(middle), CommandCategory.Settings)

    runner.groupPaths("leaf", List(root)) shouldBe List(List("root", "middle", "leaf"))
    runner.groupPaths("missing", List(root)) shouldBe Nil
  }

  it should "not stack-overflow on a genuine multi-node cycle (A -> B -> A -> ...)" in {
    val cyclic = List(alternatingCycle(overflowingDepth))

    noException should be thrownBy runner.groupPaths("does-not-exist", cyclic)
  }

  it should "not falsely prune an unrelated top-level sibling's id when descending into a different group's children" in {
    // Same regression as `findGroup`'s equivalent test above, but for the path-reporting traversal.
    val target = CommandSurfaceItem.GroupItem("target", "Target", Nil, CommandCategory.Settings)
    val dup    = CommandSurfaceItem.GroupItem("dup", "Dup", List(target), CommandCategory.Settings)
    val groupA = CommandSurfaceItem.GroupItem("group-a", "Group A", List(dup), CommandCategory.Settings)
    val groupB = CommandSurfaceItem.GroupItem("dup", "Unrelated sibling sharing id 'dup'", Nil, CommandCategory.Settings)

    runner.groupPaths("target", List(groupA, groupB)) shouldBe List(List("group-a", "dup", "target"))
  }

  /** The production `settingsGroups` tree itself: a standing invariant test that it is (and stays) acyclic, so a
    * future settings-group addition that accidentally nests a group under itself is caught here rather than only
    * being tolerated at runtime by the defensive guard above.
    */
  "CommandRunner.empty.settingsGroups" should "be acyclic -- no group id appears within its own subtree" in {
    def assertAcyclic(groups: List[CommandSurfaceItem.GroupItem], ancestry: List[String]): Unit =
      groups.foreach { group =>
        withClue(s"group '${group.id}' nested under ancestry ${(group.id :: ancestry).reverse.mkString(" > ")}: ") {
          ancestry should not contain group.id
        }
        val childGroups = group.children.collect { case child: CommandSurfaceItem.GroupItem => child }
        assertAcyclic(childGroups, group.id :: ancestry)
      }

    assertAcyclic(CommandRunner.empty.settingsGroups, Nil)
  }

  /** Regression coverage for the "once-real self-referential-parent bug" this issue's title references (see
    * `CommandRunner.exitSubmenuToPreview`'s doc comment and `CommandRunnerFocusSpec`'s "close the command runner
    * fully via repeated Escapes" test, which already encodes the fixed Escape count): reading the parent id
    * straight off `surface.ancestors.headOption` rather than a separately tracked `parentGroupId` field means a
    * top-level (ancestor-less) settings page can never compute a self-referential parent, by construction.
    */
  it should "never compute exitSubmenuToPreview's parent id as the page's own id" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val topLevelGroups    = CommandRunner.empty.activate(registry, com.serenity.config.AppConfig.default).settingsGroups
    val topLevelGroupId   = topLevelGroups.headOption.map(_.id).getOrElse(fail("expected at least one settings group"))

    val entered = CommandRunner.empty
      .activate(registry, com.serenity.config.AppConfig.default)
      .openSettings
      .withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group(topLevelGroupId, 0)))

    // A top-level page has no ancestors, so `exitSubmenuToPreview` must take the `Nil` branch (fully closing the
    // settings surface) rather than ever synthesizing `parentId == groupId`.
    entered.activeSettingsSurface.map(_.ancestors) shouldBe Some(Nil)

    val exited = entered.exitSubmenuToPreview
    exited.activeSettingsSurface shouldBe None
  }
