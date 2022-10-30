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
  /** This opaque type alias is used to associate current rendering config with
    * a particular instance of Rendering - to make sure that two different
    * renderings don't overlap
    */
  opaque type Context <: Config = Config

  /** Use this [[Rendering]] object - places the required context in the given
    * scope for methods such as line, block, deep, etc. to actually work.
    *
    * If there's already a rendering context in scope, this method will inherit
    * it, maintaining the nesting level. Otherwise it will start at the
    * indentation level associated with this instance of [[Rendering]]
    *
    * @param f
    *   rendering block
    * @param ctx
    *   rendering context, defaults to the one associated with this Rendering
    *   instance
    * @return
    */
  inline def use(f: Context ?=> Unit)(using ctx: Context = c) =
    f(using ctx)
    this
  end use

  /** Renders the block at the indentation level originally associated with this
    * Rendering instance. What it means is that no matter how many levels of
    * nest {..} you are in, they will all be ignored.
    *
    * @param f
    *   rendering block
    * @return
    */
  inline def reset(f: Context ?=> Unit): Rendering =
    f(using c)
    this
  end reset

  /** Returns the indentation whitespace at the current nesting level. This
    * method does not modify the contents of the LineBuilder
    *
    * @param c
    * @return
    */
  inline def indent(using c: Context): String =
    (" " * (c.indentSize.value * c.indents.value))

  /** Increases indentation level for all the rendering calls done inside the
    * anonymous function `f`
    *
    * @param f
    *   block with rendering calls which will have their indentation increased
    * @param context
    *   current rendering context
    * @return
    */
  inline def nest(f: Context ?=> Unit)(using context: Context): Rendering =
    f(using context.copy(indents = context.indents.map(_ + 1)))
    this
  end nest

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
  inline def deep(count: Int)(f: Context ?=> Unit)(using
      context: Context
  ): Rendering =
    f(using context.copy(indents = context.indents.map(_ + count)))
    this
  end deep

  /** Writes a line to the builder indented to the correct number of spaces
    * (depending on config and nesting)
    *
    * @param str
    *   text line to write
    * @param context
    *   current rendering context
    * @return
    */
  inline def line(str: String)(using context: Context): Rendering =
    to.appendLine(indent(using context) + str)
    this
  end line

  /** Adds empty line to the builder
    *
    * @return
    */
  inline def emptyLine(): Rendering =
    to.appendLine("")
    this

  /** Creates an instance of Rendering at the current nesting level.
    *
    * Useful if you want to extract some rendering logic into a separate
    * function/method and want it to maintain the nesting level regardless of
    * where it's called
    *
    * @param context
    * @return
    */
  inline def forkRendering(using context: Context = c): Rendering =
    Rendering(to, context)

  /** Creates a block, where `start` and `end` will be rendered as separate
    * lines at current indentation level, and the rendering block `f` will have
    * its indentation increased
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
  inline def block(start: String, end: String)(f: Context ?=> Unit)(using
      context: Context
  ): Rendering =
    line(start)
    nest { f }
    line(end)
    this
  end block

  /** Renders `items` as separate lines, with separators placed between them.
    *
    * If you use `Separator.Newline(...)`, then separator will be placed on a
    * separate line.
    *
    * If you use `Separator.Append(...)`, then separator will be added to the
    * end of the line
    *
    * @param separator
    * @param items
    * @param ctx
    * @return
    */
  inline def intersperse(
      separator: Separator
  )(items: Seq[String])(using ctx: Context): Rendering =
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
