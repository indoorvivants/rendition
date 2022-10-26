Rendition is a micro library intended to be replace basically the same code in projects like [sn-bindgen](https://sn-bindgen.indoorvivants.com/) and [langoustine](https://github.com/neandertech/langoustine/)

```scala mdoc
import rendition._

val lb = LineBuilder()

lb.render {r => 
  import r.*

  line("object test:")
  nest {
    line("def hello = ")
    nest {
      line("val x = 25")
    }
    line("def test = 5")
  }
}

val expected = 
"""
|object test:
|  def hello = 
|    val x = 25
|  def test = 5
""".stripMargin.trim

assert(expected == lb.result.trim)

```

