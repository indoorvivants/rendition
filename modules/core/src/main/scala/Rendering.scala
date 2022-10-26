/*
 * Copyright 2022 Anton Sviridov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package rendition

import Rendering.Config
import IndentationSize.*

case class Rendering(to: LineBuilder, c: Config):
  opaque type PrivateC = Config

  inline def indent(using c: PrivateC): String =
    (" " * (c.indentSize.value * c.indents.value))

  inline def nest(f: PrivateC ?=> Unit)(using config: PrivateC = c) =
    f(using config.copy(indents = config.indents.map(_ + 1)))

  inline def deep(count: Int)(f: PrivateC ?=> Unit)(using
      config: PrivateC = c
  ) =
    f(using config.copy(indents = config.indents.map(_ + count)))

  inline def line(n: String)(using config: PrivateC = c) =
    to.appendLine(indent(using config) + n)

  inline def forkRendering(using config: PrivateC = c) = Rendering(to, config)
end Rendering

object Rendering:
  case class Config(indents: Indentation, indentSize: IndentationSize)
  object Config:
    val default = Config(Indentation(0), IndentationSize(2))
