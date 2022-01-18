package funprog.error

sealed trait DonneesIncorectesException[A] {
  def showError(a: A): String
}

case object ErreurFichierInput extends DonneesIncorectesException[String] {
  def showError(err: String): String = s"Impossible de lire le fichier: $err"
}

case object ErreurNombreDeLignesInput extends DonneesIncorectesException[String] {
  def showError(nbLine: String): String = s"Il manque des données: $nbLine lignes trouvées"
}

case object ChampIncorrectInput extends DonneesIncorectesException[String] {
  def showError(fieldSizeFormat: String): String = s"Erreur au niveau des champs $fieldSizeFormat"
}

case object MowerPositionFormatIncorrect extends DonneesIncorectesException[String] {
  def showError(mowerFormat: String): String = s"Le format de la position initiale est incorrect: $mowerFormat"
}

case object InstructionFormatIncorrect extends DonneesIncorectesException[String] {
  def showError(instrFormat: String): String = s"Le format des instruction est incorrect  : $instrFormat"
}

case object AllerHorsDuJardin extends DonneesIncorectesException[(Int, Int)] {
  def showError(coordinate: (Int, Int)): String = s"La position $coordinate est en dehors du jardin"
}

case object InstructionInconnue extends DonneesIncorectesException[String] {
  def showError(instr: String): String = s"Instruction inconnue : $instr"
}

case object CognerUnMur extends DonneesIncorectesException[(Int, Int)] {
  def showError(coordinate: (Int, Int)): String = s"La tondeuse va cogner un mur au$coordinate"
}

case object CardinalInconnu extends DonneesIncorectesException[String] {
  def showError(card: String): String = s"Cardinal inconnu : $card"
}