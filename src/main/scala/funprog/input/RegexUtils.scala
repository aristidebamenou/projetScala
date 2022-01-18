package funprog.input

import scala.util.matching.Regex

object RegexUtils {

  implicit class RegexString(val reg: Regex) extends AnyVal {
    def matches(s: String): Boolean = reg.pattern.matcher(s).matches
  }

  implicit class RegexVector(val reg: Regex) extends AnyVal {
    def matches(vs: Vector[String]): Boolean = vs.forall(s => reg.pattern.matcher(s).matches)
  }

  implicit class RegexList(val reg: Regex) extends AnyVal {
    def matches(ls: List[String]): Boolean = ls.forall(s => reg.pattern.matcher(s).matches)
  }

  object RegexAnalysis {
    val patternMowerPos: Regex = """^[\t ]*(\d+)[\t ]+(\d+)[\t ]+([newsNEWS]{1})[\t ]*$""".r
    val patternFieldSize: Regex = """[\t ]*(\d+)[\t ]+(\d+)[\t ]*""".r
    val patternInstructions: Regex = """[agdAGD]+""".r
  }

}
