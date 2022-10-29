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

opaque type LineBuilder = StringBuilder
object LineBuilder:
  private val SEP                           = System.lineSeparator()
  def apply(): LineBuilder                  = new StringBuilder
  def apply(sb: StringBuilder): LineBuilder = sb
  extension (lb: LineBuilder)
    def value: StringBuilder               = lb.asInstanceOf[StringBuilder]
    def result: String                     = lb.result
    def appendLine(s: String): LineBuilder = lb.append(s + SEP)
    def emptyLine(): LineBuilder           = lb.append(SEP)
    def emptyLines(n: Int): LineBuilder    = lb.append(SEP * n)
    def append(s: String): LineBuilder     = lb.append(s)

    def rendering(
        c: Config = Config.default
    ): Rendering =
      Rendering(lb, c)

    def render(f: Rendering => Unit, c: Config = Config.default): LineBuilder =
      f(rendering(c))
      lb
  end extension
end LineBuilder
