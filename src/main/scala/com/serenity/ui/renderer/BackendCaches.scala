package com.serenity.ui.renderer

import java.util.concurrent.ConcurrentHashMap

/** Names the caches one render backend keeps for an owner. A key is compared by identity, so a backend declares exactly
  * one, and `kind` is how the caches come back with their own type.
  */
final class BackendCacheKey[A <: AnyRef](val kind: Class[A])

/** Caches keyed by backend, each created the first time its backend asks for them. */
final class CacheSlots:

  private val slots = new ConcurrentHashMap[BackendCacheKey[?], AnyRef]()

  /** The caches of `key`, made by `make` if this is the first ask. Concurrent first asks make them once, and `make` may
    * itself ask for other caches of these slots: `computeIfAbsent` cannot allow that, since it throws when two keys in
    * flight share a bin.
    */
  def getOrCreate[A <: AnyRef](key: BackendCacheKey[A])(make: => A): A =
    key.kind.cast(Option(slots.get(key)).getOrElse(createOnce(key, make)))

  private def createOnce[A <: AnyRef](key: BackendCacheKey[A], make: => A): AnyRef =
    slots.synchronized {
      Option(slots.get(key)).getOrElse {
        val made = make
        slots.put(key, made)
        made
      }
    }

  def holds(key: BackendCacheKey[?]): Boolean = slots.containsKey(key)

/** The caches render backends keep for one owner, each created the first time its backend asks for them. The owner
  * itself names no backend, so a launch that never draws with one never loads its classes. `shared` holds what a
  * backend keeps once for every owner built over it, such as the fonts it has opened.
  */
final class BackendCaches(val shared: CacheSlots = new CacheSlots):

  private val own = new CacheSlots

  def getOrCreate[A <: AnyRef](key: BackendCacheKey[A])(make: => A): A = own.getOrCreate(key)(make)

  def holds(key: BackendCacheKey[?]): Boolean = own.holds(key)
