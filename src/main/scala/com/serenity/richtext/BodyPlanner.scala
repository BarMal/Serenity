package com.serenity.richtext

/** One piece of a saved body: source markup to copy as it is, or a paragraph to write out again. */
private[richtext] enum PlannedBlock:
  case Verbatim(markup: String)
  case Rewrite(paragraph: RichTextParagraph, gap: String)

/** Decides, for each paragraph of an edited document, whether its source slice can be copied or it must be rewritten,
  * and where the body children the model does not hold (tables, section properties) go among them.
  */
private[richtext] object BodyPlanner:

  final private case class Progress(planned: Vector[PlannedBlock], nextBlock: Int, usedBlocks: Set[Int])

  def plan(paragraphs: List[RichTextParagraph], body: BodySource): Vector[PlannedBlock] =
    val done = paragraphs.foldLeft(Progress(Vector.empty, 0, Set.empty))(step(body))
    done.planned ++ verbatimBlocks(body, done.nextBlock, body.blocks.size)

  /** Every paragraph written afresh, for a package whose body could not be cut into slices. */
  def planWithoutSlices(paragraphs: List[RichTextParagraph]): Vector[PlannedBlock] =
    paragraphs.map(PlannedBlock.Rewrite(_, "\n")).toVector

  private def step(body: BodySource)(progress: Progress, paragraph: RichTextParagraph): Progress =
    val block =
      paragraph.source.map(_.blockIndex).filter(index => index >= progress.nextBlock && index < body.blocks.size)
    val leading = block.fold(Vector.empty[PlannedBlock])(verbatimBlocks(body, progress.nextBlock, _))
    val reusable = block
      .filterNot(progress.usedBlocks.contains)
      .filter(index => paragraph.source.exists(_.imported.contains(paragraph)) && body.blocks(index).isParagraph)
    val planned = reusable.fold(
      PlannedBlock.Rewrite(paragraph, block.fold("")(body.blocks(_).gap))
    )(index => PlannedBlock.Verbatim(body.blocks(index).markup))
    Progress(
      progress.planned ++ leading :+ planned,
      block.fold(progress.nextBlock)(_ + 1),
      progress.usedBlocks ++ reusable
    )

  private def verbatimBlocks(body: BodySource, from: Int, until: Int): Vector[PlannedBlock] =
    body.blocks.slice(from, until).filterNot(_.isParagraph).map(block => PlannedBlock.Verbatim(block.markup))
