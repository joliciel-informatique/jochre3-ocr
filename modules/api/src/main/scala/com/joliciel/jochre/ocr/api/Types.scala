package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import zio.{RIO, Task}

object Types {
  type Requirements = Jochre & Lexicon

  type AppTask[T] = RIO[Requirements, T]
}
