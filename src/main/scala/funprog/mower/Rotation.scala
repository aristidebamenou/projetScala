package funprog.mower

import funprog.playground.{Cardinal, East, North, South, West}

trait Rotation[A] {
  def leftRotation(a: A): A
  def rightRotation(a: A): A
}

object Rotation {

  def apply[A](implicit e: Rotation[A]): Rotation[A] = e

  def leftRotation[A: Rotation](a: A): A = Rotation[A].leftRotation(a)
  def rightRotation[A: Rotation](a: A): A = Rotation[A].rightRotation(a)

  implicit class Rotate[A: Rotation](a: A) {
    def leftRotation: A = Rotation[A].leftRotation(a)
    def rightRotation: A = Rotation[A].rightRotation(a)
  }

  implicit val CardRotation: Rotation[Cardinal] =
    new Rotation[Cardinal] {
      def leftRotation(card: Cardinal): Cardinal = card match {
        case North => West
        case East => North
        case South => East
        case West => South
      }

      def rightRotation(card: Cardinal): Cardinal = card match {
        case North => East
        case East => South
        case South => West
        case West => North
      }
    }

  implicit val MowerRotation: Rotation[Mower] =
    new Rotation[Mower] {

      def leftRotation(lm: Mower): Mower =
        lm.copy(lm.pos.copy(orientation = lm.pos.orientation.leftRotation))

      def rightRotation(lm: Mower): Mower =
        lm.copy(lm.pos.copy(orientation = lm.pos.orientation.rightRotation))
    }
}
