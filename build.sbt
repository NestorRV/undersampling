name := "undersampling"

version := "2.1.1"

organization := "com.github.NestorRV"

organizationName := "Nestor Rodriguez Vico"

organizationHomepage := Some(url("https://github.com/NestorRV/undersampling"))

scalaVersion := "2.12.5"
scalacOptions in(Compile, doc) ++= Opts.doc.title("undersampling")
scalacOptions += "-deprecation"
scalacOptions += "-unchecked"
scalacOptions += "-feature"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-stable" % "3.8.2"