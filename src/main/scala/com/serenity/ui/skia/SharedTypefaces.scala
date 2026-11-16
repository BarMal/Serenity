package com.serenity.ui.skia

import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.renderer.BackendCacheKey
import org.jetbrains.skia.Typeface

/** The typefaces resolved for every owner of one set of shared slots. A typeface is immutable data, but resolving one
  * opens or clones its font file in native memory, and each owner that measures (a window's state, its surfaces) would
  * otherwise hold its own copy of the same faces, a variable font's weight clone at every size among them.
  */
final class SharedTypefaces(
    resolve: SkiaTypefaces.TypefaceKey => Typeface = SkiaTypefaces.typefaceFor,
    limit: Int = SharedTypefaces.DefaultLimit
):

  private val resolved  = new ConcurrentHashMap[SkiaTypefaces.TypefaceKey, Typeface]()
  private val resolving = new Object

  /** `key`'s typeface, resolved the first time it is asked for. Past `limit` typefaces the holder starts again, which
    * only costs the owners that ask for a dropped one a second lookup.
    */
  def typefaceFor(key: SkiaTypefaces.TypefaceKey): Typeface =
    Option(resolved.get(key)).getOrElse {
      resolving.synchronized {
        Option(resolved.get(key)).getOrElse {
          if resolved.size >= limit then resolved.clear()
          val typeface = resolve(key)
          val _        = resolved.put(key, typeface)
          typeface
        }
      }
    }

object SharedTypefaces:

  val DefaultLimit: Int = 1024

  val Key: BackendCacheKey[SharedTypefaces] = BackendCacheKey(classOf[SharedTypefaces])
