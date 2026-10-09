package com.serenity.app

import com.serenity.BuildInfo

object VersionBanner:

  def render(version: String, commit: String): String = s"Serenity $version ($commit)"

  def current: String = render(BuildInfo.version, BuildInfo.commit)
