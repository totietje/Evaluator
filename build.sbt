import java.io.PrintWriter

name := "Evaluator"

version := "1.1.0"

//scalaVersion := "2.12.2"
crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2")

scalacOptions ++= Seq("-feature", "-deprecation")

organization := "net.totietje"

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

credentials += Credentials(Path.userHome / ".bintray" / ".credentials")

publishMavenStyle := true
name in bintray := "Evaluator"
version in bintray := version.value
bintrayRepository := "maven"

scalacOptions in (Compile, doc) ++= Seq("-sourcepath", baseDirectory.value.getAbsolutePath)

scalacOptions in (Compile, doc) ++= Seq("-doc-title", name.value,
  "-doc-version", version.value,
  "-groups",
  "-doc-source-url", s"https://github.com/totietje/evaluator/blob/v${version.value}€{FILE_PATH}.scala",
  "-doc-root-content", (baseDirectory.value / "project" / "doc-root.txt").toString)

target in Compile in doc := baseDirectory.value / "docs" / version.value

lazy val makeDocIndex = TaskKey[Unit]("makeDocIndex")

makeDocIndex := {
  val f = baseDirectory.value / "docs" / "index.html"
  f.createNewFile()
  val p = new PrintWriter(f)

  val dir = baseDirectory.value / "docs"
  val versions = dir.listFiles()
    .filter(_.isDirectory)
    .map(_.getName)
    .sorted
    .reverse
    .transform((v: String) => String.format("<li><a href=\"%s\">%s</a></li>", v, v))
    .mkString
    
  p.write(s"""<!DOCTYPE html>
             |<html>
             |    <head>
             |        <link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">
             |        <title>Evaluator Docs</title>
             |    </head>
             |    <body>
             |        <div class="w3-container">
             |            <h1>Evaluator Docs</h1>
             |            <ul class="w3-ul w3-border">
             |                <li><h3>Version:</h3></li>
             |                $versions
             |            </ul>
             |        </div>
             |    </body>
             |</html>""".stripMargin)
  p.close()
}