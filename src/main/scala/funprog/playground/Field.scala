package funprog.playground

import funprog.error.{AllerHorsDuJardin, DonneesIncorectesException}
import funprog.mower.Mower

sealed trait Cardinal
case object North extends Cardinal
case object East extends Cardinal
case object South extends Cardinal
case object West extends Cardinal

object CardinalUtils {
  implicit class StrToCardinal(val str: String) extends AnyVal {
    def toCardinal: Cardinal = str.toUpperCase match {
      case "N" => North
      case "NORTH" => North
      case "S" => South
      case "SOUTH" => South
      case "E" => East
      case "EAST" => East
      case "W" => West
      case "WEST" => West
      case _ => North
    }
  }
}

case class Field(length : Int, width : Int, mowers : List[Mower] = List[Mower]()){
  def add(lm: Mower): Either[DonneesIncorectesException[(Int, Int)], Field] =
    lm match {
      case l if !isInField(l.pos.x, l.pos.y) => Left(AllerHorsDuJardin)
      case _ => Right(Field(length, width, mowers :+ lm))
    }

  def getInvalidMowers: List[Mower] = mowers.filter(lm => !Field.isInField(Field(length, width, mowers diff List(lm)), lm))

  def isInField(x: Int, y: Int): Boolean = x >= 0 && y >=0 && x <=length && y <= width
}

object Field {
  def isInField(field: Field, lm: Mower): Boolean = lm.pos.x >= 0 && lm.pos.y >=0 && lm.pos.x <=field.length && lm.pos.y <= field.width

  implicit class Compute(field: Field) {
    def computeField: (List[Either[String, Mower]], Field) = {
      def computeFieldAcc(f: Field, l: List[Mower], res: List[Either[String, Mower]]): (List[Either[String, Mower]], Field) = {
        l match {
          case x :: xs =>
            val fieldCopy = f.copy(mowers = f.mowers diff List(x))
            val resAndField = x.executeInstructionRec(fieldCopy)
            computeFieldAcc(resAndField._2, xs, res ::: resAndField._1)
          case Nil => (res, f)
        }
      }
      computeFieldAcc(field, field.mowers, List())
    }
  }
}

case class Position(x: Int, y: Int, orientation: Cardinal)