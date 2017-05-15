name := "Evaluator"

version := "1.0.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature")

organization := "net.totietje"

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

credentials += Credentials(Path.userHome / ".bintray" / ".credentials")

publishMavenStyle := true
name in bintray := "Evaluator"
version in bintray := "1.0.0"
bintrayRepository := "maven"

target in Compile in doc := baseDirectory.value / "docs"