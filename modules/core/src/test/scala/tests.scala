import rendition.*
import scala.util.NotGiven.apply
import scala.util.NotGiven
import scala.annotation.implicitNotFound
import scala.util.boundary, boundary.break

class Tests extends munit.FunSuite:
  test("general usage") {
    val lb = LineBuilder()

    lb.use {

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

        withReset {
          line("println(25)")
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
      |println(25)
      """.trim.stripMargin

    assertEquals(lb.result.trim, expected)
  }

  test("intersperse") {
    val lb = LineBuilder()
    lb.use {

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

    def f(using RenderingContext) =
      line("I come from a function!")
      nest { line("and me") }

    lb.use {
      line("object hello:")
      nest {
        line("object test:")
        nest {
          f
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

  test("deep nesting") {
    val lb1 = LineBuilder()
    val lb2 = LineBuilder()

    lb1.use {
      line("test")
      deep(3) {
        line("hello")
      }
    }

    lb2.use {
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

  test("transaction") {
    val lb1 = LineBuilder()

    var capturedError = Option.empty[String]

    lb1.use:
      line("Hello")
      line("World")
      nest:
        capturedError = transact[String]:
          line("start")
          break("oopsie daisy")
          line("end")

        transact[String]:
          line("ok")
          line("now")
          nest:
            line("go")
      line("home")

    assertEquals(Some("oopsie daisy"), capturedError)

    val expected =
      """
      |Hello
      |World
      |  ok
      |  now
      |    go
      |home
      """.stripMargin.trim

    assertEquals(expected, lb1.result.trim)

  }
end Tests
