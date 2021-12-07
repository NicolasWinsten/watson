name := "watson"

version := "0.1"

scalaVersion := "2.12.6"

idePackagePrefix := Some("com.nicolaswinsten.watson")
libraryDependencies += "org.apache.lucene" % "lucene-core" % "8.11.0"
libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "8.11.0"
libraryDependencies += "org.apache.lucene" % "lucene-queryparser" % "8.11.0"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "4.3.1"
libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "4.3.1" classifier "models"