name := "undersampling"

version := "1.0"

organization := "com.github.NestorRV"

organizationName := "Nestor Rodriguez Vico"

organizationHomepage := Some(url("https://github.com/NestorRV/"))

scalaVersion := "2.12.4"
scalacOptions in(Compile, doc) ++= Opts.doc.title("undersampling")
scalacOptions += "-deprecation"
scalacOptions += "-unchecked"
scalacOptions += "-feature"