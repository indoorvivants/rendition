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

enum Separator:
  case Newline(s: String)
  case Append(s: String)

case class Rendering(to: LineBuilder, c: Config):
  opaque type Context = Config

  inline def indent(using c: Context): String =
    (" " * (c.indentSize.value * c.indents.value))

  inline def nest(f: Context ?=> Unit)(using context: Context = c) =
    f(using context.copy(indents = context.indents.map(_ + 1)))

  inline def deep(count: Int)(f: Context ?=> Unit)(using
      context: Context = c
  ) =
    f(using context.copy(indents = context.indents.map(_ + count)))

  inline def line(n: String)(using context: Context = c) =
    to.appendLine(indent(using context) + n)

  inline def emptyLine()(using context: Context = c) =
    line("")

  inline def forkRendering(using context: Context = c) = Rendering(to, context)

  inline def block(start: String, end: String)(f: Context ?=> Unit)(using
      context: Context = c
  ) =
    line(start)
    nest { f }
    line(end)

  inline def intersperse(
      separator: Separator
  )(iterator: Seq[String])(using ctx: Context = c) =
    if iterator.size < 2 then iterator.foreach(line(_))
    else
      var separators = iterator.size - 1
      iterator.foreach { str =>
        separator match
          case Separator.Newline(s) =>
            line(str)
            if separators > 0 then
              line(s)
              separators -= 1
          case Separator.Append(s) =>
            if separators > 0 then
              line(str + s)
              separators -= 1
            else line(str)

      }
end Rendering

object Rendering:
  case class Config(indents: Indentation, indentSize: IndentationSize)
  object Config:
    val default = Config(Indentation(0), IndentationSize(2))

  opaque type IndentationSize = Int
  object IndentationSize extends OpaqueNum[IndentationSize]

  opaque type Indentation = Int
  object Indentation extends OpaqueNum[Indentation]

  import IndentationSize.*
end Rendering
