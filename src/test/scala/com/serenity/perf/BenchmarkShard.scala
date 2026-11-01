package com.serenity.perf

/** One contiguous slice of the suite, `index` of `count` (1-based), so that CI can run half of it in each of two jobs
  * and each shard keeps the suite's order (the warm-up budget is spent in that order).
  */
final private[perf] case class BenchmarkShard(index: Int, count: Int):

  def select[A](rows: List[A]): List[A] =
    rows.slice(rows.size * (index - 1) / count, rows.size * index / count)

private[perf] object BenchmarkShard:

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
