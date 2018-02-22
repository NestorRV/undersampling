name := "undersampling"

version := "2.0.1"

organization := "com.github.NestorRV"

organizationName := "Nestor Rodriguez Vico"

organizationHomepage := Some(url("https://github.com/NestorRV/undersampling"))

scalaVersion := "2.12.4"
scalacOptions in(Compile, doc) ++= Opts.doc.title("undersampling")
scalacOptions += "-deprecation"
scalacOptions += "-unchecked"
scalacOptions += "-feature"