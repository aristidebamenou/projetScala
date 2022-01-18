package funprog.input

import scala.util.Try
import scala.io.Source
import com.typesafe.config.{Config, ConfigFactory}
import funprog.error.{ChampIncorrectInput, ErreurNombreDeLignesInput, InstructionFormatIncorrect, MowerPositionFormatIncorrect}
import funprog.input.RegexUtils.RegexAnalysis
import funprog.mower.Mower
import funprog.playground.CardinalUtils.StrToCardinal
import funprog.playground.{Field, Position}

object File {
  val conf: Config = ConfigFactory.load()

  def getInput: Try[List[String]] = {
    val input = Source.fromFile(conf.getString("application.input-file"))
    val results = Try(input.getLines().toList)
    input.close()
    results
  }

  def analyzeFormat(content: List[String]): Either[List[String], Field] = {
    val contentSize = content.length
    val invalidSizeFormat: Option[String] = isValidNumberOfLine(contentSize)
    val errorFieldSizeFormat: Option[String] = errorSizeField(content.head)

    val sizeField: Option[(Int,Int)] = errorFieldSizeFormat match {
      case None =>
        val RegexAnalysis.patternFieldSize(length, width) = content.head
        Some(length.toInt, width.toInt)
      case Some(_) => None
    }

    val mower = content.drop(1)
    val positions: List[String] = getEvenIndexElement(mower)
    val instructions: List[String] = getOddIndexElement(mower)

    val posAnalysisIncorrect: List[String] =  positions.filter(instr => !isMowerPosValidFormat(instr))
    val posAnalysisErrors: List[Option[String]] = posAnalysisIncorrect.map(s =>
      Some(MowerPositionFormatIncorrect.showError(s))
    )

    val instrAnalysisIncorrect: List[String] =  instructions.filter(instr => !isMowerPosValidFormat(instr))
    val instrAnalysisErrors: List[Option[String]] = instrAnalysisIncorrect.map(s =>
      Some(InstructionFormatIncorrect.showError(s))
    )

    val errors = List(
      List(invalidSizeFormat),
      List(errorFieldSizeFormat),
      isNonEmptyErrorsList(posAnalysisErrors).getOrElse(List()),
      isNonEmptyErrorsList(instrAnalysisErrors).getOrElse(List())
    )

    val flatErrors = errors.flatten.flatten

    if(flatErrors.nonEmpty)
      Left(flatErrors)

    else {
      val mowerTuples = positions zip instructions
      val mowers = mowerTuples.map{
        case(pos, instr) =>
          val RegexAnalysis.patternMowerPos(x, y, orientation) = pos
          Mower(Position(x.toInt, y.toInt, orientation.toCardinal), instr)
      }
      Right(Field(sizeField.get._1, sizeField.get._2, mowers))
    }

  }

  def isValidNumberOfLine(contentSize: Int): Option[String] = contentSize match {
    case c if c < 3 || c%2 == 0 => Some(ErreurNombreDeLignesInput.showError(c.toString))
    case _ => None
  }

  def errorSizeField(content: String): Option[String] = content match {
    case c if !isFieldValidFormat(c) => Some(ChampIncorrectInput.showError(c))
    case _ => None
  }

  def isFieldValidFormat(s: String): Boolean = RegexAnalysis.patternFieldSize matches s

  def getEvenIndexElement(l: List[String]): List[String] = l.indices.collect { case i if i % 2 == 0 => l(i) }.toList

  def getOddIndexElement(l: List[String]): List[String] = l.indices.collect { case i if i % 2 != 0 => l(i) }.toList

  def isMowerPosValidFormat(s: String): Boolean = RegexAnalysis.patternMowerPos matches s

  def isNonEmptyErrorsList(content: List[Option[String]]): Option[List[Option[String]]] = content match {
    case c if c.nonEmpty => Some(c)
    case _ => None
  }

}
