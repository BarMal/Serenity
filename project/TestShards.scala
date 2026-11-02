package com.serenity.build

import java.nio.charset.StandardCharsets

/** Splits `sbt test` across CI jobs by suite (#2484): a job given shard `i/n` runs the suites that shard `i` owns.
  *
  * A suite's shard comes from `project/test-shards.tsv` (suite, shard, seconds), which is built from measured suite
  * times so the shards weigh about the same. A suite the table does not list falls back to a fixed hash of its name, so
  * a new spec runs in exactly one shard without anyone editing the table; `check` is what keeps the table from listing
  * suites that no longer exist.
  *
  * Lives under `project/` because the build calls it, and is also compiled into the test sources so a spec can reach
  * it; the meta-build is Scala 2.12, so this file sticks to syntax both compilers read.
  */
object TestShards {

  final case class Shard(index: Int, of: Int)

  final case class Entry(shard: Int, seconds: Double)

  final case class Summary(suites: Map[Int, Int], seconds: Map[Int, Double])

  def parseShard(spec: String): Either[String, Shard] =
    spec.split("/", -1).toList match {
      case i :: n :: Nil =>
        (toInt(i), toInt(n)) match {
          case (Some(index), Some(of)) if of >= 1 && index >= 1 && index <= of => Right(Shard(index, of))
          case _ => Left(s"shard '$spec' must be index/of with 1 <= index <= of")
        }
      case _ => Left(s"shard '$spec' must be index/of with 1 <= index <= of")
    }

  def parseTable(lines: Seq[String]): Either[String, Map[String, Entry]] =
    lines
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .foldLeft[Either[String, Map[String, Entry]]](Right(Map.empty)) { (acc, line) =>
        acc.flatMap { table =>
          line.split("\t", -1).toList match {
            case suite :: shard :: seconds :: Nil =>
              (toInt(shard).filter(_ >= 1), toDouble(seconds)) match {
                case (Some(s), Some(t)) =>
                  if (table.contains(suite)) Left(s"suite $suite is listed twice")
                  else Right(table.updated(suite, Entry(s, t)))
                case _ => Left(s"suite $suite has a shard or seconds that is not a number: '$line'")
              }
            case _ => Left(s"expected suite<TAB>shard<TAB>seconds: '$line'")
          }
        }
      }

  def shardOf(table: Map[String, Entry], suite: String, of: Int): Int =
    table.get(suite).map(_.shard).getOrElse((fnv1a(suite) % of).toInt + 1)

  def select(table: Map[String, Entry], shard: Shard, suite: String): Boolean =
    shardOf(table, suite, shard.of) == shard.index

  def check(table: Map[String, Entry], defined: Seq[String], of: Int): Either[List[String], Summary] = {
    val known = defined.toSet
    val stale = table.keys
      .filterNot(known.contains)
      .toList
      .sorted
      .map(s => s"$s is in the shard table but is not a test suite; regenerate the table")
    val beyond = table.toList.collect {
      case (suite, e) if e.shard > of => s"$suite is on shard ${e.shard} but there are only $of shards"
    }.sorted
    val errors = stale ++ beyond
    if (errors.nonEmpty) Left(errors)
    else {
      val owner  = defined.map(suite => suite -> shardOf(table, suite, of))
      val shards = 1 to of
      Right(
        Summary(
          suites = shards.map(s => s -> owner.count(_._2 == s)).toMap,
          seconds = shards
            .map(s => s -> owner.collect { case (suite, `s`) => table.get(suite).map(_.seconds).getOrElse(0.0) }.sum)
            .toMap
        )
      )
    }
  }

  /** Longest-first onto the lightest shard, ties broken by name so the table is reproducible. */
  def balance(seconds: Map[String, Double], of: Int): Map[String, Int] =
    seconds.toList
      .sortBy { case (suite, t) => (-t, suite) }
      .foldLeft((Map.empty[String, Int], Vector.fill(of)(0.0))) {
        case ((assigned, load), (suite, t)) =>
          val lightest = load.indexOf(load.min)
          (assigned.updated(suite, lightest + 1), load.updated(lightest, load(lightest) + t))
      }
      ._1

  def secondsFromReports(xmls: Seq[String]): Map[String, Double] = {
    val suite = """<testsuite\b[^>]*?\bname="([^"]+)"[^>]*?\btime="([0-9.]+)"""".r
    xmls.flatMap(xml => suite.findFirstMatchIn(xml).map(m => m.group(1) -> m.group(2).toDouble)).toMap
  }

  def render(entries: Map[String, Entry]): String =
    entries.toList
      .sortBy { case (suite, e) => (e.shard, -e.seconds, suite) }
      .map { case (suite, e) => s"$suite\t${e.shard}\t${e.seconds}" }
      .mkString("\n") + "\n"

  private def fnv1a(text: String): Long =
    text.getBytes(StandardCharsets.UTF_8).foldLeft(2166136261L) { (hash, byte) =>
      ((hash ^ (byte & 0xff)) * 16777619L) & 0xffffffffL
    }

  private def toInt(text: String): Option[Int]       = scala.util.Try(text.trim.toInt).toOption
  private def toDouble(text: String): Option[Double] = scala.util.Try(text.trim.toDouble).toOption
}
