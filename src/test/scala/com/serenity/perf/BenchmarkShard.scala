package com.serenity.perf

/** One contiguous slice of the suite, `index` of `count` (1-based), so that CI can run part of it in each of several
  * jobs and each shard keeps the suite's order (the warm-up budget is spent in that order).
  *
  * Slices are equal except for two shards: the rows of the first half of the suite cost more than the second half's
  * (one unsharded run, 132 timed rows: half of the time the rows themselves took, leaving out the warm-up extension,
  * was spent by row 61, i.e. at 46%), so the first shard takes the first 40% and the second the rest. That figure is
  * from one run and moves as rows are added.
  *
  * 40, not 45, since CI run 41996744074 (#2618): at 45 the 1x first shard's base+head pair cost about 644 s against
  * about 457 s for the second, and its job ran 47.8 min against 38.3. The 2x gap was smaller (42.2 against 37.5 min),
  * so one constant cannot balance both scales; 40 sits between what each wanted.
  */
final private[perf] case class BenchmarkShard(index: Int, count: Int):

  def select[A](rows: List[A]): List[A] =
    rows.slice(boundary(index - 1, rows.size), boundary(index, rows.size))

  private def boundary(slice: Int, size: Int): Int =
    if count == 2 && slice == 1 then size * BenchmarkShard.FirstOfTwoPercent / 100
    else size * slice / count

private[perf] object BenchmarkShard:

  val FirstOfTwoPercent: Int = 40

  private val Option = "--shard="

  def parse(text: String): Either[String, BenchmarkShard] =
    text.split("/", -1).toList match
      case index :: count :: Nil =>
        (index.toIntOption, count.toIntOption) match
          case (Some(i), Some(c)) if c >= 1 && i >= 1 && i <= c => Right(BenchmarkShard(i, c))
          case _ => Left(s"not a shard: '$text' (expected index/count, 1 <= index <= count)")
      case _ => Left(s"not a shard: '$text' (expected index/count)")

  /** The `--shard=i/n` option, if there is one, and the other arguments (benchmark names) as they were. */
  def fromArgs(args: List[String]): Either[String, (scala.Option[BenchmarkShard], List[String])] =
    val (options, names) = args.partition(_.startsWith(Option))
    options.headOption.map(option => parse(option.stripPrefix(Option))) match
      case Some(Left(error))  => Left(error)
      case Some(Right(shard)) => Right((Some(shard), names))
      case None               => Right((None, names))
