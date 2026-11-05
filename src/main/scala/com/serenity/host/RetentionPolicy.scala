package com.serenity.host

import java.time.Instant

import scala.concurrent.duration.*

/** What a kept host may cost. `idle` zero means a host is stopped the moment it is detached, which is today's behaviour
  * of stopping servers on quit.
  */
final case class RetentionLimits(idle: FiniteDuration, perHostBytes: Long, totalBytes: Long)

object RetentionLimits:

  private val DefaultIdle         = 10.minutes
  private val DefaultPerHostBytes = 3L * 1024 * 1024 * 1024
  private val DefaultTotalCeiling = 4L * 1024 * 1024 * 1024
  private val DefaultTotalShare   = 4

  /** 10 minutes idle, 3 GiB per host, and in total 4 GiB or a quarter of the machine's memory, whichever is less. */
  def defaults(physicalMemoryBytes: Long): RetentionLimits =
    RetentionLimits(DefaultIdle, DefaultPerHostBytes, DefaultTotalCeiling.min(physicalMemoryBytes / DefaultTotalShare))

enum HostLink:
  case Attached
  case Detached(since: Instant)

/** One host as the retention check sees it: whether a Serenity is attached, and what its processes hold. */
final case class HostStatus(host: HostKey, link: HostLink, residentBytes: Long)

enum StopReason:
  case IdleExpired, OverHostCap, OverTotalCap

final case class StopOrder(host: HostKey, reason: StopReason)

/** Something to tell the user because an attached host is never stopped for its size. */
enum RetentionNotice:
  case AttachedOverHostCap(host: HostKey, residentBytes: Long, capBytes: Long)
  case TotalOverCap(residentBytes: Long, capBytes: Long)

final case class RetentionDecision(stops: List[StopOrder], notices: List[RetentionNotice])

/** Which kept hosts to stop. The caps decide what is kept after a client leaves; they never stop an attached host.
  *
  * Time is wall-clock, so a host that slept past its expiry is stopped at the first check after waking.
  */
object RetentionPolicy:

  def decide(limits: RetentionLimits, now: Instant, hosts: List[HostStatus]): RetentionDecision =
    val ownStops  = hosts.sortBy(_.host.id).flatMap(ownStop(limits, now, _))
    val stopped   = ownStops.map(_.host).toSet
    val remaining = hosts.filterNot(status => stopped.contains(status.host))
    val evictions = evictForTotal(limits.totalBytes, remaining)
    val evicted   = evictions.map(_.host).toSet
    val kept      = remaining.filterNot(status => evicted.contains(status.host))
    RetentionDecision(ownStops ++ evictions, notices(limits, kept))

  private def ownStop(limits: RetentionLimits, now: Instant, status: HostStatus): Option[StopOrder] =
    status.link match
      case HostLink.Attached => None
      case HostLink.Detached(since) =>
        val expiry = since.plus(java.time.Duration.ofNanos(limits.idle.toNanos))
        if !now.isBefore(expiry) then Some(StopOrder(status.host, StopReason.IdleExpired))
        else if status.residentBytes > limits.perHostBytes then Some(StopOrder(status.host, StopReason.OverHostCap))
        else None

  private def evictForTotal(totalCap: Long, remaining: List[HostStatus]): List[StopOrder] =
    val leastRecentlyDetachedFirst = remaining
      .flatMap(status =>
        status.link match
          case HostLink.Detached(since) => Some((since, status))
          case HostLink.Attached        => None
      )
      .sortBy((since, status) => (since, status.host.id))
      .map((_, status) => status)
    val start = (remaining.map(_.residentBytes).sum, List.empty[StopOrder])
    leastRecentlyDetachedFirst
      .foldLeft(start) {
        case ((total, orders), status) =>
          if total > totalCap then
            (total - status.residentBytes, StopOrder(status.host, StopReason.OverTotalCap) :: orders)
          else (total, orders)
      }
      ._2
      .reverse

  private def notices(limits: RetentionLimits, kept: List[HostStatus]): List[RetentionNotice] =
    val oversizedAttached = kept
      .filter(status => status.link == HostLink.Attached && status.residentBytes > limits.perHostBytes)
      .sortBy(_.host.id)
      .map(status => RetentionNotice.AttachedOverHostCap(status.host, status.residentBytes, limits.perHostBytes))
    val total = kept.map(_.residentBytes).sum
    val machine =
      if total > limits.totalBytes then List(RetentionNotice.TotalOverCap(total, limits.totalBytes)) else Nil
    oversizedAttached ++ machine
