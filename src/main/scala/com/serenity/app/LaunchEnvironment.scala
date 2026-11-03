package com.serenity.app

import java.util.Locale

import com.serenity.config.{RenderBackendSetting, SettingChoices, WaylandRender}

/** The environment variables that choose how the window is drawn. A value one of them cannot make sense of stops the
  * launch with a message, as an invalid flag does, instead of quietly meaning something else.
  */
object LaunchEnvironment:

  final private case class Variable(name: String, choices: List[String], accepts: String => Boolean)

  private val variables = List(
    Variable(
      RenderBackendSelection.OverrideVariable,
      SettingChoices.RenderBackendKeys,
      RenderBackendSetting.fromConfigKey(_).isDefined
    ),
    Variable(
      RenderBackendSelection.WaylandOverrideVariable,
      SettingChoices.WaylandRenderKeys,
      WaylandRender.fromConfigKey(_).isDefined
    ),
    Variable(ToolkitSelection.OverrideVariable, ToolkitSelection.Accepted, ToolkitSelection.Accepted.contains)
  )

  /** One message per variable that is set to something unusable. A blank value counts as unset. */
  def problems(env: Map[String, String]): List[String] =
    variables.flatMap { variable =>
      env
        .get(variable.name)
        .map(_.trim)
        .filter(_.nonEmpty)
        .filterNot(value => variable.accepts(value.toLowerCase(Locale.ROOT)))
        .map(value =>
          s"${variable.name} is '$value', which is not valid; expected ${SettingChoices.phrase(variable.choices)}."
        )
    }
