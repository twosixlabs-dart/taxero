package org.clulab.taxero

import java.io.{BufferedWriter, File, FileWriter}

import ai.lum.common.ConfigFactory
import com.typesafe.config.Config

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object OntologyExtender extends App {
  // given a directory with ontology files, where each file has a set of examples for one node of the ontology
  // produces corresponding files with the example sets enriched with hyponyms and co-hyponyms

  // get the reader
  val config: Config = ConfigFactory.load()
  val reader = TaxonomyReader.fromConfig

  // create directories and files
  val ontologyDirectory = new File("/home/alexeeva/Repos/taxero/src/main/resources/ontologyFiles")
  val outputDir = new File(ontologyDirectory.getAbsolutePath + "UpdatedWithHyponyms")
  outputDir.mkdir()
  val files = ontologyDirectory.listFiles()

  for (file <- files) {
    val outfile = new File(outputDir, file.getName.replace(".txt", "UpdatedWithHyponyms.txt"))
    // retrieve existing examples
    val examples = Source.fromFile(file).getLines().toList

    val resultsFromAllTerms = new ArrayBuffer[String]()

    for (eg <- examples.filter(!_.startsWith("#")).filter(_.length > 0)) {
        val egAsSeq = eg.split(" ")
        val results = reader.getRankedHyponyms(egAsSeq, lemmatize = false).map(res => res.result.mkString(" ")).toList.distinct
        val coResults = reader.getRankedCohyponyms(egAsSeq, lemmatize = false).map(res => res.result.mkString(" ")).toList.distinct // can have cohyp for found hyponyms
        for (r <-results) resultsFromAllTerms.append(r)
        for (r <-coResults) resultsFromAllTerms.append(r)

    }
    val bw = new BufferedWriter(new FileWriter(outfile))
    // write existing examples to the output file
    bw.write(examples.filter(_.length > 0).mkString("\n") + "\n")
    // write new examples
    bw.write(resultsFromAllTerms.distinct.filter(_.length > 0).mkString("\n"))
    bw.close()
  }
}
