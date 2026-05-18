scalaVersion := "3.8.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Sistema Operativo não suportado!")
}

// Módulos do JavaFX necessários
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics")

libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "17.0.2" classifier osName
)