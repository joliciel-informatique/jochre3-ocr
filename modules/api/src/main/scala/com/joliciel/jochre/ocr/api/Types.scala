package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.core.Jochre
import com.joliciel.jochre.ocr.core.lexicon.Lexicon
import zio.{RIO, Task}
import com.joliciel.jochre.ocr.core.text.Dehyphenator

object Types {
  type Requirements = Jochre & Lexicon & Dehyphenator

  type AppTask[T] = RIO[Requirements, T]
}
