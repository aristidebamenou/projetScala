package funprog

import com.typesafe.config.{Config, ConfigFactory}
import funprog.input.File
import funprog.error.{ErreurFichierInput, InstructionInconnue}
import funprog.mower.Mower
import funprog.playground.Field

import scala.util.{Failure, Success}

object Main extends App {
  val conf: Config = ConfigFactory.load()
  println(s"${conf.getString("application.name")}")

  File.getInput match {
    case Success(informations) =>
      val resultLines = File.analyzeFormat(informations)
      resultLines match {
        case Left(e) => println(e)
        case Right(field) =>

          val cleanFieldInitial: Field = field.copy(mowers = field.mowers)
          val fieldComputeAndField: (List[Either[String, Mower]], Field) = cleanFieldInitial.computeField
          val fieldComputeRes: List[Either[String, Mower]] = fieldComputeAndField._1
          val finalField: Field = fieldComputeAndField._2

          fieldComputeRes.foreach {
            case Left(impossibleInstr) => println(InstructionInconnue.showError(impossibleInstr))
            case Right(vehicleFinalState) => println(vehicleFinalState)
          }

          val displayVehicle = cleanFieldInitial.mowers zip finalField.mowers

          displayVehicle.foreach { case (v1, v2) => s"${v1.pos.show} => ${v2.pos.show}".loggingResult() }
      }
    case Failure(exception) => println(ErreurFichierInput.showError(exception.getMessage))
  }

}
