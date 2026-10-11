package com.serenity.state.models

import java.nio.file.Path

/** The files that matched `query` in a [[FileFinder]] listing, in listing order: what a longer query starting with
  * `query` can narrow from.
  */
final case class FileMatches(query: String, files: Vector[Path])

/** Ranking a listing's files against a typed query for [[FileFinder]]. */
object FileMatching:

  type Scorer = (String, String) => Option[Int]

  private val BestFirst: Ordering[(Path, Int)] = Ordering.by { case (file, score) => (-score, file.toString) }

  /** The best `limit` files for `query`, best first, and every file that matched. Only the files `previous` matched are
    * scored when `query` extends its query: [[FuzzyMatch]] needs every query character in order, so a longer query can
    * only match where its prefix did.
    */
  def ranked(
    files: Vector[Path],
    query: String,
    previous: Option[FileMatches],
    limit: Int,
    score: Scorer = FuzzyMatch.score
  ): (Vector[Path], FileMatches) =
    val scored =
      candidates(files, query, previous).flatMap(file => score(query, FileFinder.slashed(file)).map(file -> _))
    (TopK.smallest(scored, limit)(using BestFirst).map(_._1), FileMatches(query, scored.map(_._1)))

  /** The files `query` needs to be scored against. */
  def candidates(files: Vector[Path], query: String, previous: Option[FileMatches]): Vector[Path] =
    previous.filter(matched => startsWithIgnoringCase(query, matched.query)).fold(files)(_.files)

  // Case is ignored per character, as FuzzyMatch ignores it, so "SER" extends "se".
  private def startsWithIgnoringCase(query: String, prefix: String): Boolean =
    query.length >= prefix.length && prefix.indices.forall(index => query(index).toLower == prefix(index).toLower)
