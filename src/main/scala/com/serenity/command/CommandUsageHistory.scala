package com.serenity.command

/** The palette's recency table (#1877): registry command ids, valued by a generation that is higher the more recently
  * the command ran. Capped at [[Capacity]], least recent evicted first, so neither the table nor the session file it is
  * saved to grows without bound.
  */
object CommandUsageHistory:

  val Capacity: Int = 50

  def recorded(usage: Map[CommandId, Int], id: CommandId): Map[CommandId, Int] =
    val nextGeneration = usage.values.maxOption.getOrElse(0) + 1
    mostRecent(usage + (id -> nextGeneration))

  /** A table read back from a session file, which may predate this cap or hold keys that were never commands -- an
    * option-cycling intent's `toString`, a settings row id -- so anything `isRegistered` rejects is dropped.
    */
  def restored(usage: Map[CommandId, Int], isRegistered: CommandId => Boolean): Map[CommandId, Int] =
    mostRecent(usage.filter { case (id, _) => isRegistered(id) })

  private def mostRecent(usage: Map[CommandId, Int]): Map[CommandId, Int] =
    if usage.size <= Capacity then usage
    else usage.toList.sortBy { case (_, generation) => -generation }.take(Capacity).toMap
