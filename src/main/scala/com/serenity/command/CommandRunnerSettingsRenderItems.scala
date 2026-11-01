package com.serenity.command

import com.serenity.config.{RenderBackendSetting, RenderDamageGranularity, RenderFpsTarget, WaylandRender}

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

  private[command] def renderBackendOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = "render-backend",
      label = "Renderer",
      options = List(
        CommandOption(
          "Auto",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderBackend(RenderBackendSetting.Auto))
          )
        ),
        CommandOption(
          "Skia",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderBackend(RenderBackendSetting.Skia))
          )
        ),
        CommandOption(
          "Java2D",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderBackend(RenderBackendSetting.Java2D))
          )
        )
      ),
      selectedIndex = optionSelections.getOrElse("render-backend", 0),
      category = CommandCategory.Settings,
      hint = Some("Takes effect after restarting; Skia needs an X11 display on Linux")
    )

  private[command] def renderWaylandOptionItem(
    optionSelections: Map[String, Int]
  ): CommandSurfaceItem.OptionItem =
    def option(label: String, mode: WaylandRender) =
      CommandOption(
        label,
        CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetRenderWayland(mode)))
      )
    CommandSurfaceItem.OptionItem(
      id = "render-wayland",
      label = "Wayland Renderer",
      options = List(
        option("Java2D", WaylandRender.Java2D),
        option("Skia (CPU)", WaylandRender.SkiaRaster),
        option("Skia via XWayland", WaylandRender.XWayland)
      ),
      selectedIndex = optionSelections.getOrElse("render-wayland", 1),
      category = CommandCategory.Settings,
      hint = Some("Only when Skia would run on a Wayland session; takes effect after restarting")
    )
