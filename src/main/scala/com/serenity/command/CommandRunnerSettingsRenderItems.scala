package com.serenity.command

import com.serenity.config.{RenderDamageGranularity, RenderFpsTarget}

private[command] object CommandRunnerSettingsRenderItems:

  private[command] def renderFpsOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "render-fps",
      label = "Render FPS",
      options = List(
        CommandOption(
          "30 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps30))
          )
        ),
        CommandOption(
          "60 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps60))
          )
        ),
        CommandOption(
          "90 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps90))
          )
        ),
        CommandOption(
          "120 FPS",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120))
          )
        ),
        CommandOption(
          "Uncapped",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Uncapped))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("render-fps", 1),
      category = CommandCategory.Settings,
      hint = Some("Render loop cadence")
    )

  private[command] def renderDamageGranularityOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "render-damage-granularity",
      label = "Repaint Granularity",
      options = List(
        CommandOption(
          "Rows",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderDamageGranularity(RenderDamageGranularity.Rows))
          )
        ),
        CommandOption(
          "Cells",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderDamageGranularity(RenderDamageGranularity.Cells))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("render-damage-granularity", 0),
      category = CommandCategory.Settings,
      hint = Some("Cells applies to monospaced code buffers only")
    )
