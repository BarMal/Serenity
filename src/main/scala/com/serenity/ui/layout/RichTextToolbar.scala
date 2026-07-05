package com.serenity.ui.layout

/** A formatting action shown by the rich text toolbar. */
case class RichTextToolbarItem(
    id: String,
    label: String,
    commandName: String
)

/** Transient toolbar state kept independent from app reducers until the UI workflow is wired. */
case class RichTextToolbarState(
    anchor: Option[ScreenPosition],
    focusedIndex: Int = 0
):

  def isVisible: Boolean =
    anchor.nonEmpty

  def showAt(position: ScreenPosition): RichTextToolbarState =
    copy(anchor = Some(position), focusedIndex = focusedIndex.max(0))

  def dismiss: RichTextToolbarState =
    RichTextToolbarState.Hidden

  def focusedItem(items: List[RichTextToolbarItem]): Option[RichTextToolbarItem] =
    items.lift(clampedFocusIndex(items))

  def focusNext(items: List[RichTextToolbarItem]): RichTextToolbarState =
    moveFocus(1, items)

  def focusPrevious(items: List[RichTextToolbarItem]): RichTextToolbarState =
    moveFocus(-1, items)

  private def moveFocus(delta: Int, items: List[RichTextToolbarItem]): RichTextToolbarState =
    if items.isEmpty then copy(focusedIndex = 0)
    else
      val rawIndex = (clampedFocusIndex(items) + delta) % items.length
      copy(focusedIndex = if rawIndex < 0 then rawIndex + items.length else rawIndex)

  private def clampedFocusIndex(items: List[RichTextToolbarItem]): Int =
    if items.isEmpty then 0 else focusedIndex.max(0).min(items.length - 1)

object RichTextToolbarState:
  val Hidden: RichTextToolbarState = RichTextToolbarState(anchor = None)

  def visible(anchor: ScreenPosition): RichTextToolbarState =
    Hidden.showAt(anchor)

enum RichTextToolbarPlacement:
  case AboveCursor
  case BelowCursor

/** Calculated toolbar rectangle and the side of the cursor where it was placed. */
case class RichTextToolbarLayout(
    rect: LayoutRect,
    placement: RichTextToolbarPlacement
)

/** Cell dimensions and cursor gap for the first toolbar layout contract. */
case class RichTextToolbarLayoutConfig(
    width: Int = RichTextToolbarLayoutConfig.DefaultWidth,
    height: Int = RichTextToolbarLayoutConfig.DefaultHeight,
    gapRows: Int = RichTextToolbarLayoutConfig.DefaultGapRows
):

  def normalized: RichTextToolbarLayoutConfig =
    copy(
      width = width.max(1),
      height = height.max(1),
      gapRows = gapRows.max(0)
    )

object RichTextToolbarLayoutConfig:
  val DefaultWidth: Int   = 44
  val DefaultHeight: Int  = 3
  val DefaultGapRows: Int = 1

object RichTextToolbar:

  val defaultItems: List[RichTextToolbarItem] = List(
    RichTextToolbarItem("bold", "Bold", "bold"),
    RichTextToolbarItem("italic", "Italic", "italic"),
    RichTextToolbarItem("underline", "Underline", "underline"),
    RichTextToolbarItem("paragraph-body", "Body", "paragraph-body"),
    RichTextToolbarItem("heading-1", "H1", "heading-1"),
    RichTextToolbarItem("heading-2", "H2", "heading-2"),
    RichTextToolbarItem("heading-3", "H3", "heading-3"),
    RichTextToolbarItem("align-left", "Left", "align-left"),
    RichTextToolbarItem("align-center", "Center", "align-center"),
    RichTextToolbarItem("align-right", "Right", "align-right"),
    RichTextToolbarItem("align-justify", "Justify", "align-justify")
  )

  def layout(
    state: RichTextToolbarState,
    contentRect: LayoutRect,
    config: RichTextToolbarLayoutConfig = RichTextToolbarLayoutConfig()
  ): Option[RichTextToolbarLayout] =
    state.anchor.filter(_ => contentRect.width > 0 && contentRect.height > 0).map { anchor =>
      val normalized = config.normalized
      val width      = normalized.width.min(contentRect.width)
      val height     = normalized.height.min(contentRect.height)
      val x          = clamp(anchor.x - width / 2, contentRect.x, contentRect.right - width)
      val aboveY     = anchor.y - height - normalized.gapRows
      val belowY     = anchor.y + 1 + normalized.gapRows

      if aboveY >= contentRect.y then
        RichTextToolbarLayout(
          rect = LayoutRect(x, aboveY, width, height),
          placement = RichTextToolbarPlacement.AboveCursor
        )
      else if belowY + height <= contentRect.bottom then
        RichTextToolbarLayout(
          rect = LayoutRect(x, belowY, width, height),
          placement = RichTextToolbarPlacement.BelowCursor
        )
      else
        RichTextToolbarLayout(
          rect = LayoutRect(
            x = x,
            y = clamp(aboveY, contentRect.y, contentRect.bottom - height),
            width = width,
            height = height
          ),
          placement = RichTextToolbarPlacement.AboveCursor
        )
    }

  private def clamp(value: Int, min: Int, max: Int): Int =
    value.max(min).min(max)
