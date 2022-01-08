package funprog

import com.typesafe.config.{Config, ConfigFactory}

object Main extends App {
  println("Ici le programme principal")
  val conf: Config = ConfigFactory.load()
}
