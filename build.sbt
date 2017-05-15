name := "Evaluator"

version := "1.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature")
target in Compile in doc := baseDirectory.value / "docs"