package com.serenity.ui.widget

import com.serenity.keystroke.events.{Enter, InsertChar, MoveToStart, PageDown, ScrollUp, ToggleSyntaxHighlighting}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WidgetModelsSpec extends AnyFlatSpec with Matchers:

  private val rows = 5

  "WidgetInput" should "translate the editor's key events into widget inputs" in {
    WidgetInput.fromEvent(Enter) shouldBe Some(WidgetInput.Activate)
    WidgetInput.fromEvent(PageDown) shouldBe Some(WidgetInput.PageDown)
    WidgetInput.fromEvent(MoveToStart) shouldBe Some(WidgetInput.First)
    WidgetInput.fromEvent(ScrollUp(3)) shouldBe Some(WidgetInput.Scroll(-3))
    WidgetInput.fromEvent(InsertChar('x')) shouldBe Some(WidgetInput.Insert('x'))
    WidgetInput.fromEvent(ToggleSyntaxHighlighting) shouldBe None
  }

  it should "read Space as a toggle only when asked to" in {
    WidgetInput.asToggle(WidgetInput.Insert(' ')) shouldBe WidgetInput.Toggle
    WidgetInput.asToggle(WidgetInput.Insert('x')) shouldBe WidgetInput.Insert('x')
  }

  "A multi-select list" should "toggle the selected item with Space and confirm the checked items in list order" in {
    val list = MultiSelectList.of(Vector("a", "b", "c"))
    val checked =
      Seq(WidgetInput.Down, WidgetInput.Down, WidgetInput.Insert(' '), WidgetInput.First, WidgetInput.Toggle)
        .foldLeft(list)((current, input) => current.update(input, rows)._1)

    checked.checkedItems shouldBe Vector("a", "c")
    checked.update(WidgetInput.Activate, rows)._2 shouldBe Some(MultiSelectOutcome.Confirmed(Vector("a", "c")))
    checked.update(WidgetInput.Insert(' '), rows)._1.checkedItems shouldBe Vector("c")
  }

  it should "check everything, or clear everything once all are checked" in {
    val list = MultiSelectList.of(Vector("a", "b"))
    list.checkAll.checkedItems shouldBe Vector("a", "b")
    list.checkAll.checkAll.checkedItems shouldBe Vector.empty
  }

  it should "toggle the clicked item" in {
    MultiSelectList.of(Vector("a", "b")).update(WidgetInput.Click(1, 1), rows)._1.checkedItems shouldBe Vector("b")
  }

  "A text field" should "insert at the caret and replace a selection" in {
    val field = TextField.of("helo").movedTo(3)
    field.update(WidgetInput.Insert('l'))._1 shouldBe TextField("hello", 4)
    TextField.of("hello").update(WidgetInput.SelectAll)._1.update(WidgetInput.Insert('x'))._1 shouldBe TextField("x", 1)
  }

  it should "report a change only when the text changed" in {
    TextField.of("a").update(WidgetInput.Insert('b'))._2 shouldBe Some(TextFieldOutcome.Changed("ab"))
    TextField.of("").update(WidgetInput.DeleteBackward)._2 shouldBe None
    TextField.of("a").update(WidgetInput.Activate)._2 shouldBe Some(TextFieldOutcome.Submitted("a"))
  }

  it should "delete and step over whole grapheme clusters" in {
    val flag = "🇬🇧"
    TextField.of(s"a$flag").update(WidgetInput.DeleteBackward)._1 shouldBe TextField("a", 1)
    TextField.of(s"a$flag").update(WidgetInput.Left)._1.caret shouldBe 1
  }

  it should "delete and move by words" in {
    TextField.of("one two").update(WidgetInput.DeleteWordBackward)._1.text shouldBe "one "
    TextField.of("one two").update(WidgetInput.WordLeft)._1.caret shouldBe 4
    TextField.of("one two").update(WidgetInput.First)._1.caret shouldBe 0
  }

  it should "collapse a selection to its edge on Left and Right" in {
    val selected = TextField("hello", caret = 4, anchor = Some(1))
    selected.update(WidgetInput.Left)._1 shouldBe TextField("hello", 1)
    selected.update(WidgetInput.Right)._1 shouldBe TextField("hello", 4)
  }

  it should "keep pasted text on one line" in {
    TextField().update(WidgetInput.InsertText("a\nb"))._1.text shouldBe "a b"
  }

  private val tree = TreeView.of(
    Vector(
      TreeNode(
        "src",
        "src",
        TreeChildren.Loaded(Vector(TreeNode("main", "main", TreeChildren.Unloaded), TreeNode("a.md", "a.md")))
      ),
      TreeNode("b.md", "b.md")
    )
  )

  private def keysOf(view: TreeView[String, String]): Vector[String] = view.rows.items.map(_.node.key)

  "A tree" should "expand with Right, step into its first child, and collapse or step out with Left" in {
    val expanded = tree.update(WidgetInput.Right, rows)._1
    keysOf(expanded) shouldBe Vector("src", "main", "a.md", "b.md")
    val inside = expanded.update(WidgetInput.Right, rows)._1
    inside.selectedNode.map(_.key) shouldBe Some("main")
    val backOut = inside.update(WidgetInput.Left, rows)._1
    backOut.selectedNode.map(_.key) shouldBe Some("src")
    keysOf(backOut.update(WidgetInput.Left, rows)._1) shouldBe Vector("src", "b.md")
  }

  it should "ask for an unloaded branch's children, showing it as loading meanwhile" in {
    val onMain       = tree.update(WidgetInput.Right, rows)._1.update(WidgetInput.Down, rows)._1
    val (loading, o) = onMain.update(WidgetInput.Right, rows)
    o shouldBe Some(TreeOutcome.LoadChildren("main"))
    loading.selectedNode.map(_.children) shouldBe Some(TreeChildren.Loading)

    val loaded = loading.withChildren("main", TreeChildren.Loaded(Vector(TreeNode("x.scala", "x.scala"))), rows)
    keysOf(loaded) shouldBe Vector("src", "main", "x.scala", "a.md", "b.md")
    loaded.selectedNode.map(_.key) shouldBe Some("main")
  }

  it should "retry a branch whose children failed to load" in {
    val failed = tree.withChildren("src", TreeChildren.Failed("denied"), rows)
    failed.update(WidgetInput.Right, rows)._2 shouldBe Some(TreeOutcome.LoadChildren("src"))
  }

  it should "activate a leaf, and toggle a branch, on Enter or a double-click" in {
    val onLeaf = tree.update(WidgetInput.Last, rows)._1
    onLeaf.update(WidgetInput.Activate, rows)._2 shouldBe Some(TreeOutcome.Activated(TreeNode("b.md", "b.md")))
    keysOf(tree.update(WidgetInput.Click(0, 2), rows)._1) shouldBe Vector("src", "main", "a.md", "b.md")
  }

  private val fruit = Dropdown.of(Vector("apple", "banana", "cherry"), identity[String], chosen = Some("banana"))

  "A dropdown" should "open on the chosen option and choose with Enter" in {
    val open = fruit.update(WidgetInput.Activate, rows)._1
    open.open shouldBe true
    open.list.selectedItem shouldBe Some("banana")
    val (closed, chosen) = open.update(WidgetInput.Down, rows)._1.update(WidgetInput.Activate, rows)
    chosen shouldBe Some(DropdownOutcome.Chosen("cherry"))
    closed.chosen shouldBe Some("cherry")
    closed.open shouldBe false
  }

  it should "close on Escape without choosing" in {
    val (closed, outcome) = fruit.opened(rows).update(WidgetInput.Down, rows)._1.update(WidgetInput.Dismiss, rows)
    outcome shouldBe Some(DropdownOutcome.Dismissed)
    closed.chosen shouldBe Some("banana")
  }

  it should "narrow its options by typing when it is a combo box" in {
    val combo = Dropdown.of(Vector("apple", "banana", "cherry"), identity[String], filterable = true).opened(rows)
    val typed = Seq('a', 'n').foldLeft(combo)((current, char) => current.update(WidgetInput.Insert(char), rows)._1)
    typed.list.items shouldBe Vector("banana")
    typed.update(WidgetInput.Activate, rows)._2 shouldBe Some(DropdownOutcome.Chosen("banana"))
  }

  "A button" should "fire its action on Enter, Space or a click, unless disabled" in {
    val save = Button("Save", "save", ButtonEmphasis.Primary)
    save.pressed(WidgetInput.Activate) shouldBe Some("save")
    save.pressed(WidgetInput.Insert(' ')) shouldBe Some("save")
    save.pressed(WidgetInput.Click(0, 1)) shouldBe Some("save")
    save.copy(disabledReason = Some("Nothing to save")).pressed(WidgetInput.Activate) shouldBe None
  }

  "A checkbox" should "flip on Space, unless disabled" in {
    Checkbox("Wrap", checked = false).update(WidgetInput.Insert(' ')).checked shouldBe true
    Checkbox("Wrap", checked = false, Some("locked")).update(WidgetInput.Toggle).checked shouldBe false
  }

  "A radio group" should "choose the highlighted option on Space" in {
    val group            = RadioGroup.of(Vector("left", "right"), chosen = Some("left"))
    val (chosen, choice) = group.update(WidgetInput.Down, rows)._1.update(WidgetInput.Toggle, rows)
    choice shouldBe Some("right")
    chosen.chosen shouldBe Some("right")
  }

  "Tabs" should "switch with Left and Right, wrapping, and report the newly active tab" in {
    val tabs = Tabs.of(Vector("General", "Keys", "Theme"))
    tabs.update(WidgetInput.Right)._2 shouldBe Some("Keys")
    tabs.update(WidgetInput.Left)._2 shouldBe Some("Theme")
    tabs.update(WidgetInput.Click(0, 1))._2 shouldBe None
  }

  "A focus ring" should "move with Tab and Shift+Tab, wrapping" in {
    val ring = FocusRing.of(Vector("name", "ok", "cancel"))
    ring.next.focused shouldBe Some("ok")
    ring.previous.focused shouldBe Some("cancel")
    ring.focus("cancel").next.focused shouldBe Some("name")
    ring.focus("missing") shouldBe ring
  }

  "A progress bar" should "fill exactly its width, in eighths of a cell" in {
    ProgressGlyphs.bar(Progress.Determinate(0.0), 4) shouldBe "░░░░"
    ProgressGlyphs.bar(Progress.Determinate(0.5), 4) shouldBe "██░░"
    ProgressGlyphs.bar(Progress.Determinate(0.5625), 4) shouldBe "██▎░"
    ProgressGlyphs.bar(Progress.Determinate(1.0), 4) shouldBe "████"
    ProgressGlyphs.bar(Progress.Determinate(7.0), 4) shouldBe "████"
    ProgressGlyphs.percent(Progress.Determinate(0.5)) shouldBe Some("50%")
  }

  it should "sweep a block along the track while indeterminate" in {
    ProgressGlyphs.bar(Progress.Indeterminate, 10, frame = 0) shouldBe "██░░░░░░░░"
    ProgressGlyphs.bar(Progress.Indeterminate, 10, frame = 3) shouldBe "░░░██░░░░░"
    ProgressGlyphs.bar(Progress.Indeterminate, 10, frame = 8) shouldBe "░░░░░░░░██"
    ProgressGlyphs.bar(Progress.Indeterminate, 10, frame = 10).length shouldBe 10
    ProgressGlyphs.percent(Progress.Indeterminate) shouldBe None
  }

  "A loadable value" should "map only a ready value" in {
    Loadable.Ready(2).map(_ * 2) shouldBe Loadable.Ready(4)
    Loadable.Failed("no").map((n: Int) => n * 2) shouldBe Loadable.Failed("no")
    Loadable.Empty("none").toOption shouldBe None
  }
end WidgetModelsSpec
