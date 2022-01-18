package funprog.mower

import funprog.error.CognerUnMur
import funprog.mower.Rotation.Rotate
import funprog.playground.{East, Field, North, Position, South, West}

case class Mower(pos: Position, instruction : String) {
  def moveForward(): Mower = pos.orientation match {
    case North => Mower(Position(pos.x, pos.y + 1, pos.orientation), instruction)
    case South => Mower(Position(pos.x, pos.y - 1, pos.orientation), instruction)
    case East => Mower(Position(pos.x + 1, pos.y, pos.orientation), instruction)
    case West => Mower(Position(pos.x - 1, pos.y, pos.orientation), instruction)
  }

  def tryToMoveForward(field: Field): Option[String] = {
    moveForward() match {
      case v if !field.isInField(v.pos.x, v.pos.y) => Some(CognerUnMur.showError((pos.x, pos.y)))
      case _ => None
    }
  }

  def executeInstructionRec(field: Field): (List[Either[String, Mower]], Field) = {
    def executeInstructionRecAcc(field: Field, v: List[Either[String, Mower]] , instr: List[Char]): (List[Either[String, Mower]], Field) = {
      instr match {
        case x :: xs => x match {
          case 'A'  =>
            v.last match {
              case Right(lm) =>
                Mower.tryToMoveForward(field, lm) match {
                  case None =>

                    val lastRight: Option[Either[String, Mower]] = v.filter(x => x match {
                      case Left(_) => false
                      case Right(_) => true
                    }).lastOption

                    lastRight match {
                      case Some(lm2) =>  lm2 match {
                        case Right(l) => executeInstructionRecAcc (field, v :+ Right(Mower.moveForward(l)), xs)
                        case Left(_) => executeInstructionRecAcc (field, v, xs)
                      }
                      case None => executeInstructionRecAcc(field,v , xs)
                    }
                  case Some(error) => executeInstructionRecAcc(field, v :+ Left(error), xs)
                }
              case Left(error) => executeInstructionRecAcc(field, v :+ Left(error), xs)
            }
          case 'D' =>
            val lastRight = Mower.getLastRight(v)
            lastRight match {
              case Some(lm) =>  lm match {
                case Right(l) => executeInstructionRecAcc(field, v :+ Right(l.rightRotation), xs)
                case Left(_) => executeInstructionRecAcc(field, v, xs)
              }
              case None => executeInstructionRecAcc(field, v, xs)
            }
          case 'G' =>
            val lastRight = Mower.getLastRight(v)
            lastRight match {
              case Some(lm) =>  lm match {
                case Right(l) => executeInstructionRecAcc(field, v :+ Right(l.leftRotation), xs)
                case Left(_) => executeInstructionRecAcc(field, v, xs)
              }
              case None => executeInstructionRecAcc(field, v, xs)
            }
        }
        case Nil =>
          val lastRight = Mower.getLastRight(v)
          lastRight match {
            case Some(lm) =>  lm match {
              case Right(l) =>  field.add(l) match {
                case Right(f) => (v, f)
                case Left(_) => (v, field)
              }
              case Left(_) => (v, field)
            }
            case None => (v, field)
          }


      }
    }
    executeInstructionRecAcc(field, List(Right(Mower(Position(pos.x, pos.y, pos.orientation), instruction))), instruction.toUpperCase.toList)

  }

}

object Mower {
  def getLastRight(v: List[Either[String, Mower]]) : Option[Either[String, Mower]] = {
    v.filter(x => x match {
      case Left(_) => false
      case Right(_) => true
    }).lastOption
  }

  def moveForward(lm: Mower): Mower = lm.pos.orientation match {
    case North => Mower(Position(lm.pos.x, lm.pos.y + 1, lm.pos.orientation), lm.instruction)
    case South => Mower(Position(lm.pos.x, lm.pos.y - 1, lm.pos.orientation), lm.instruction)
    case East => Mower(Position(lm.pos.x + 1, lm.pos.y, lm.pos.orientation), lm.instruction)
    case West => Mower(Position(lm.pos.x - 1, lm.pos.y, lm.pos.orientation), lm.instruction)
  }

  def tryToMoveForward(field: Field, lm: Mower): Option[String] = {
    moveForward(lm) match {
      case v if !field.isInField(v.pos.x, v.pos.y) => Some(CognerUnMur.showError((lm.pos.x, lm.pos.y)))
      case _ => None
    }
  }
}
