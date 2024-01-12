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

import scala.util.boundary

import boundary.Label
import Rendering.Config

enum Separator:
  case Newline(s: String)
  case Append(s: String)

private[rendition] def context(using r: RenderingContext) = r
private[rendition] def to(using r: RenderingContext)      = r.to
private[rendition] def config(using r: RenderingContext)  = r.config
private[rendition] def originalConfig(using r: RenderingContext) =
  r.original

case class RenderingContext(to: LineBuilder, config: Config, original: Config)

/** Renders the block at the indentation level originally associated with this
  * Rendering instance. What it means is that no matter how many levels of nest
  * {..} you are in, they will all be ignored.
  *
  * @param f
  *   rendering block
  * @return
  */
inline def withReset(f: RenderingContext ?=> Unit)(using
    RenderingContext
): Unit =
  f(using RenderingContext(to, originalConfig, originalConfig))
end withReset

/** Returns the indentation whitespace at the current nesting level. This method
  * does not modify the contents of the LineBuilder
  *
  * @param c
  * @return
  */
inline def indent(using RenderingContext): String =
  (" " * (config.indentSize.value * config.indents.value))

/** Increases indentation level for all the rendering calls done inside the
  * anonymous function `f`
  *
  * @param f
  *   block with rendering calls which will have their indentation increased
  * @param context
  *   current rendering context
  * @return
  */
inline def nest(f: RenderingContext ?=> Unit)(using RenderingContext) =
  f(using
    context.copy(config =
      context.config.copy(indents = context.config.indents.map(_ + 1))
    )
  )

/** A shorter alternative to `nest { nest { nest {...` to increase indentation
  * level by necessary value
  *
  * @param count
  *   number of indentation levels to add to the current one
  * @param f
  *   block with rendering calls which will have their indentation increased
  * @param context
  * @return
  */
inline def deep(
    count: Int
)(f: RenderingContext ?=> Unit)(using RenderingContext) =
  f(using
    context.copy(config =
      context.config.copy(indents = context.config.indents.map(_ + count))
    )
  )
end deep

inline def append(what: LineBuilder)(using RenderingContext) =
  to.append(what)

/** Writes a line to the builder indented to the correct number of spaces
  * (depending on config and nesting)
  *
  * @param str
  *   text line to write
  * @param context
  *   current rendering context
  * @return
  */
inline def line(str: String)(using RenderingContext) =
  to.appendLine(indent(using context) + str)
end line

/** Adds empty line to the builder
  *
  * @return
  */
inline def emptyLine()(using RenderingContext) =
  to.appendLine("")
  this

/** @param f
  * @param context
  * @return
  */
inline def transact[A](
    f: Label[Outcome[A]] ?=> RenderingContext ?=> Outcome[A]
)(using RenderingContext): Option[A] =
  val temp   = LineBuilder()
  val forked = context.copy(to = temp)
  val result =
    boundary:
      given RenderingContext = forked
      f

  result match
    case Outcome.Ok =>
      to.append(temp)
      None
    case Outcome.Fail(reason) =>
      Some(reason)
end transact

/** Creates a block, where `start` and `end` will be rendered as separate lines
  * at current indentation level, and the rendering block `f` will have its
  * indentation increased
  *
  * @param start
  *   block header
  * @param end
  *   block footer
  * @param f
  *   rendering block with increased indentation
  * @param context
  * @return
  */
inline def block(start: String, end: String)(f: RenderingContext ?=> Unit)(using
    RenderingContext
) =
  line(start)
  nest { f }
  line(end)
end block

/** Renders `items` as separate lines, with separators placed between them.
  *
  * If you use `Separator.Newline(...)`, then separator will be placed on a
  * separate line.
  *
  * If you use `Separator.Append(...)`, then separator will be added to the end
  * of the line
  *
  * @param separator
  * @param items
  * @param ctx
  * @return
  */
inline def intersperse(
    separator: Separator
)(items: Seq[String])(using RenderingContext) =
  if items.size < 2 then items.foreach(line(_))
  else
    var separators = items.size - 1
    val onEach: String => Unit =
      separator match
        case Separator.Newline(s) =>
          (str: String) =>
            line(str)
            if separators > 0 then
              line(s)
              separators -= 1

        case Separator.Append(s) =>
          (str: String) =>
            if separators > 0 then
              line(str + s)
              separators -= 1
            else line(str)

    items.foreach(onEach)
  end if
  this
end intersperse

object Rendering:
  case class Config(indents: Indentation, indentSize: IndentationSize)
  object Config:
    val default = Config(Indentation(0), IndentationSize(2))

  opaque type IndentationSize = Int
  object IndentationSize extends OpaqueNum[IndentationSize]

  opaque type Indentation = Int
  object Indentation extends OpaqueNum[Indentation]
end Rendering

enum Outcome[+A]:
  case Ok
  case Fail(reason: A)

  def hoist(using Label[A]): Unit = this match
    case Ok => ()
    case Fail(reason) =>
      boundary.break(reason)
