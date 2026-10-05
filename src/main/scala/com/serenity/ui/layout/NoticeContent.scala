package com.serenity.ui.layout

import com.serenity.state.models.{Notice, NoticeLevel}

/** How a corner notice (#1717) is laid out: its level as a header, the message wrapped at word boundaries, and the keys
  * for what to do next as a muted footer. Sizing and painting both read these rows, so the frame always fits them.
  */
private[layout] object NoticeContent:

  /** Wide enough for a file name and a cause on one line, narrow enough to stay clear of the text being worked on. */
  val MaxFrameWidth: Int = 60

  private val BorderCells = SurfaceFrameLayout.DefaultBorderCells

  // A cell beyond the text, so a GUI frame's text inset never clips the last letter.
  private val ChromeCells = 2 * BorderCells + 1

  def frameWidth(notice: Notice, available: Int): Int =
    val longest = (notice.level.label :: notice.message :: notice.hint.toList)
      .map(_.length)
      .foldLeft(0)((widest, length) => math.max(widest, length))
    math.max(0, math.min(math.min(available, MaxFrameWidth), longest + ChromeCells))

  def frameHeight(notice: Notice, frameWidth: Int): Int =
    1 + messageLines(notice, frameWidth).size + notice.hint.size + 2 * BorderCells

  def resolve(notice: Notice, frame: LayoutRect): ResolvedSurfaceContent =
    ResolvedSurfaceContent(
      header = Some(toned(notice.level.label, toneFor(notice.level))),
      rows = messageLines(notice, frame.width).map(OverlayRow(_)),
      footer = notice.hint.map(toned(_, OverlayTone.Muted))
    )

  private def toned(text: String, tone: OverlayTone): OverlayRow =
    OverlayRow(text, segments = List(OverlaySegment(text, tone = tone)))

  private def toneFor(level: NoticeLevel): OverlayTone =
    level match
      case NoticeLevel.Error   => OverlayTone.Error
      case NoticeLevel.Warning => OverlayTone.Accent
      case NoticeLevel.Info    => OverlayTone.Normal

  private def messageLines(notice: Notice, frameWidth: Int): List[String] =
    wrapped(notice.message, math.max(1, frameWidth - ChromeCells))

  private def wrapped(text: String, width: Int): List[String] =
    text
      .split(' ')
      .toList
      .filter(_.nonEmpty)
      .foldLeft(List.empty[String]) {
        case (current :: done, word) if current.length + 1 + word.length <= width => s"$current $word" :: done
        case (lines, word)                                                        => word :: lines
      }
      .reverse match
      case Nil   => List("")
      case lines => lines
