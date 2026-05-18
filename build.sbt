scalaVersion := "3.8.1"
fork := true // Adicionado aqui

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => 
    val arch = System.getProperty("os.arch")
    if (arch == "aarch64" || arch == "arm64") "mac-aarch64" 
    else "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Sistema Operativo não suportado!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics")

libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "21.0.2" classifier osName
)

run / connectInput := true