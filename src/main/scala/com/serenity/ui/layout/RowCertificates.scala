package com.serenity.ui.layout

import java.util.concurrent.{ConcurrentHashMap, Executor}

/** The [[NominalAdvances]] each font may have its lines counted by, and the fonts that have lost theirs. One for a font
  * set without ligatures is exact and measured on the spot. One for a font set with ligatures is built by `builder` the
  * first time the font is asked about (see [[LigatureCertificate]]) and is absent until it is ready, so a line is
  * wrapped meanwhile and no frame waits for it. A certificate that a painted row contradicts is revoked and stays so.
  */
final private[serenity] class RowCertificates(builder: Executor):

  private val exact = MeasurementMemo[NominalAdvances.Key, Option[NominalAdvances]](maxEntries = 64)(key =>
    NominalAdvances.of(key.font, key.frc)
  )

  private val built     = new ConcurrentHashMap[NominalAdvances.Key, Option[NominalAdvances]]()
  private val requested = ConcurrentHashMap.newKeySet[NominalAdvances.Key]()
  private val revoked   = ConcurrentHashMap.newKeySet[NominalAdvances.Key]()

  def of(key: NominalAdvances.Key): Option[NominalAdvances] =
    if revoked.contains(key) then None else exact(key).orElse(shapedFor(key))

  /** Withdraws the font's certificate for good. True for the call that did, so a miscount is reported once. */
  def revoke(key: NominalAdvances.Key): Boolean =
    val first = revoked.add(key)
    val _     = built.remove(key)
    if first then
      RowCertificates.log.log(
        System.Logger.Level.WARNING,
        s"Row-count certificate revoked for ${key.font}: a painted line has more rows than its advances say; every line is now wrapped"
      )
    first

  private def shapedFor(key: NominalAdvances.Key): Option[NominalAdvances] =
    Option(built.get(key)) match
      case Some(done) => done
      case None =>
        if requested.add(key) then builder.execute(() => build(key))
        None

  private def build(key: NominalAdvances.Key): Unit =
    val _ = built.put(key, LigatureCertificate.of(key.font, key.frc))

private object RowCertificates:
  val log: System.Logger = System.getLogger("com.serenity.ui.layout.RowCertificates")
