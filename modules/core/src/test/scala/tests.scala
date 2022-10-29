import rendition.*

class Tests extends munit.FunSuite:
  test("general usage") {
    val lb = LineBuilder()

    lb.render { r =>
      import r.*

      line("object hello:")
      nest {
        line("def hello = ")
        nest {
          line("val x = 25")
        }
        line("def t = 5")
        block("given Test =", "end") {
          line("def x = 1")
        }

      }
    }

    val expected =
      """
      |object hello:
      |  def hello = 
      |    val x = 25
      |  def t = 5
      |  given Test =
      |    def x = 1
      |  end
      """.trim.stripMargin

    assertEquals(expected, lb.result.trim)
  }

  test("intersperse") {
    val lb = LineBuilder()
    lb.render { r =>
      import r.*

      val definitions = List.tabulate(5)(i => s"val test$i = $i")
      val arguments   = List.tabulate(5)(i => s"x$i: String")

      block("object hello:", "end hello") {
        intersperse(Separator.Newline("// test"))(definitions)
      }

      block("def function(", ") = ???") {
        intersperse(Separator.Append(","))(arguments)
      }

    }

    val expected = """
    |object hello:
    |  val test0 = 0
    |  // test
    |  val test1 = 1
    |  // test
    |  val test2 = 2
    |  // test
    |  val test3 = 3
    |  // test
    |  val test4 = 4
    |end hello
    |def function(
    |  x0: String,
    |  x1: String,
    |  x2: String,
    |  x3: String,
    |  x4: String
    |) = ???
    """.stripMargin.trim

    assertEquals(lb.result.trim, expected)

  }

  /** Forking is useful when you want to extract part of the rendering logic
    * into a separate function
    */
  test("rendering forking") {

    val lb = LineBuilder()

    def f(r: Rendering) =
      r.line("I come from a function!")
      r.nest { r.line("and me") }

    lb.render { r =>
      import r.*

      line("object hello:")
      nest {
        line("object test:")
        nest {
          f(forkRendering)
        }
      }
    }

    val expected =
      """
      |object hello:
      |  object test:
      |    I come from a function!
      |      and me
      """.trim.stripMargin

    assertEquals(lb.result.trim, expected)
  }

  test("different instances of rendering don't inherit nesting") {
    val lb = LineBuilder()
    val r1 = lb.rendering()
    val r2 = lb.rendering()

    r1.nest {
      r2.line("r2 hello")
      r1.line("r1 hello")
      r2.nest {
        r2.line("r2 bye")
        r1.line("r1 bye")
      }
    }

    r1.line("yo1")
    r2.line("yo2")

    val expected =
      """
      |r2 hello
      |  r1 hello
      |  r2 bye
      |  r1 bye
      |yo1
      |yo2
      """.trim.stripMargin

    assertEquals(lb.result.trim, expected)
  }

  test("deep nesting") {
    val lb1 = LineBuilder()
    val lb2 = LineBuilder()

    lb1.render { r =>
      import r.*
      line("test")
      r.deep(3) {
        line("hello")
      }
    }

    lb2.render { r =>
      import r.*
      line("test")
      nest {
        nest {
          nest {
            line("hello")
          }
        }
      }
    }

    assertEquals(lb1.result, lb2.result)
  }
end Tests
