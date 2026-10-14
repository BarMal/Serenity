package com.serenity.config

import java.awt.Color

/** What the terminal UI blends translucent theme colours with: a terminal cannot show what is behind a cell, so a
  * colour with alpha between 0 and 255 is mixed towards the backdrop instead.
  */
final case class TuiConfig(backdrop: Color = Color.BLACK)
