package com.serenity.config

/** A setting that takes its own value in each [[AppMode]]. */
final case class PerMode[A](code: A, prose: A):

  def forMode(mode: AppMode): A =
    mode match
      case AppMode.Code  => code
      case AppMode.Prose => prose

  def updated(mode: AppMode, value: A): PerMode[A] =
    mode match
      case AppMode.Code  => copy(code = value)
      case AppMode.Prose => copy(prose = value)

object PerMode:

  def both[A](value: A): PerMode[A] = PerMode(value, value)
