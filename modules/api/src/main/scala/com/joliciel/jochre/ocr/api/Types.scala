package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.core.Jochre
import zio.{RIO, Task}

object Types {
  type Requirements = Jochre

  type AppTask[T] = RIO[Requirements, T]
}
