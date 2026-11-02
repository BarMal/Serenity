package com.serenity.config

/** How a setting that takes one of a few names says which names, for the messages about a value that is not one. */
object SettingChoices:

  val RenderBackendKeys: List[String] = RenderBackendSetting.values.toList.map(_.configKey)
  val WaylandRenderKeys: List[String] = WaylandRender.values.toList.map(_.configKey)

  /** "a, b or c". */
  def phrase(choices: List[String]): String =
    choices.reverse match
      case Nil           => ""
      case single :: Nil => single
      case last :: rest  => s"${rest.reverse.mkString(", ")} or $last"

  /** What the loader says about a rejected value of `key`, which is then replaced by `default`. */
  def rejection(key: String, default: String): String =
    val why = key match
      case "ui.render.backend" => s"expected ${phrase(RenderBackendKeys)}"
      case "ui.render.wayland" => s"expected ${phrase(WaylandRenderKeys)}"
      case _                   => "not a value this setting accepts"
    s"$why, using its default ($default)"
