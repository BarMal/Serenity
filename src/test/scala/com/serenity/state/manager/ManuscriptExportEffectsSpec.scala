package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.io.FileDialog
import com.serenity.manuscript.SourceDocument
import com.serenity.publish.ExportOrigin
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Rope
import com.serenity.state.models.{AppState, Buffer, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

class ManuscriptExportEffectsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val immediateLanes = new EffectLanePort:
    def submitEffect(lane: com.serenity.state.effects.Lane.Keyed, job: IO[Unit]): IO[Unit]    = job
    def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] = IO.unit

  private def dialog(choice: Option[Path], asked: Ref[IO, List[(Option[Path], Option[String])]]): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(None),
      chooseSaveFile = (directory, name) => asked.update(_ :+ (directory -> name)).as(choice)
    )

  private def stateWith(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers + (buffer.id -> buffer))
    )

  private def focusedBuffer: Buffer =
    AppState.initial.focusedBufferId
      .flatMap(AppState.initial.persisted.buffers.get)
      .getOrElse(fail("no focused buffer"))

  private def markdownBuffer(path: Path, text: String): Buffer =
    val buffer = focusedBuffer
    buffer.copy(document = buffer.document.withContent(Rope(text)).copy(filePath = Some(path)))

  "ManuscriptExportEffects" should "offer a name beside the draft and write a snapshot of the buffer" in {
    val draft  = Path.of("/books/novel.md").toAbsolutePath
    val target = Path.of("/books/out.docx").toAbsolutePath
    val run = for
      asked   <- Ref.of[IO, List[(Option[Path], Option[String])]](Nil)
      written <- Ref.of[IO, List[(ExportOrigin, Path)]](Nil)
      effects = ManuscriptExportEffects(
        NoOpLogger[IO],
        Some(dialog(Some(target), asked)),
        immediateLanes,
        (origin, path) => written.update(_ :+ (origin -> path))
      )
      _         <- effects.exportFocused(stateWith(markdownBuffer(draft, "# One\n\nText.")))
      questions <- asked.get
      writes    <- written.get
    yield (questions, writes)

    val (questions, writes) = run.unsafeRunSync()

    questions shouldBe List(Some(draft.getParent) -> Some("novel-manuscript.docx"))
    writes shouldBe List(ExportOrigin(Some(draft), SourceDocument.Markdown("# One\n\nText.")) -> target)
  }

  it should "write nothing when the dialog is cancelled, and log rather than raise a failed export" in {
    val run = for
      asked   <- Ref.of[IO, List[(Option[Path], Option[String])]](Nil)
      written <- Ref.of[IO, Int](0)
      cancelled = ManuscriptExportEffects(
        NoOpLogger[IO],
        Some(dialog(None, asked)),
        immediateLanes,
        (_, _) => written.update(_ + 1)
      )
      failing = ManuscriptExportEffects(
        NoOpLogger[IO],
        Some(dialog(Some(Path.of("out.docx")), asked)),
        immediateLanes,
        (_, _) => IO.raiseError(RuntimeException("disk full"))
      )
      _      <- cancelled.exportFocused(AppState.initial)
      result <- failing.exportFocused(AppState.initial).attempt
      count  <- written.get
    yield (count, result)

    run.unsafeRunSync() shouldBe (0, Right(()))
  }

  "ManuscriptExportEffects.originOf" should "export a rich buffer's formatting only while it is in sync with the text" in {
    val document = RichTextDocument(
      List(RichTextParagraph.plain("One", role = ParagraphRole.Heading(1)), RichTextParagraph.plain("Text."))
    )
    val base   = markdownBuffer(Path.of("novel.docx"), "One\nText.")
    val inSync = base.copy(richText = base.richText.withSyncedDocument(Some(document), base.document.contentVersion))
    val stale  = inSync.copy(document = inSync.document.withContent(Rope("One\nText, edited.")))

    ManuscriptExportEffects.originOf(inSync).snapshot shouldBe SourceDocument.Rich(document)
    ManuscriptExportEffects.originOf(stale).snapshot shouldBe SourceDocument.Markdown("One\nText, edited.")
  }
