name := "etn-scw3"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq (
	"org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

retrieveManaged := true

EclipseKeys.relativizeLibs := true

