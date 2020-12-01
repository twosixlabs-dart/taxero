package org.clulab.taxero

import java.io.{BufferedWriter, File, FileWriter}

import ai.lum.common.ConfigFactory
import com.typesafe.config.Config

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object OntologyExtender extends App {
  // given a directory with ontology files, where each file has a set of examples for one node of the ontology
  // produces corresponding files with the example sets enriched with hyponyms and co-hyponyms


  // return just new
  // rank by vector similarity
  // see if hyponyms and co-hyponyms are diff quality


  def checkIfQueryIsRelevant(results: Seq[ScoredMatch], fullHeader: Seq[String]): Boolean = {
//    val hypernyms = results.slice(0,3) //for the top three results for term
//      .map(res => reader.getRankedHypernyms(res.result, lemmatize = true).head) // find its top hypernym
//      .map(_.result).flatten
    val hypernymResults = for {
      res <- results.slice(0,3)
      hypernymRes = reader.getRankedHypernyms(res.result, lemmatize = true)
      if hypernymRes.nonEmpty
    } yield hypernymRes.head.result

    if (hypernymResults.nonEmpty) {
      val hypernymTermsLemmatized = reader.convertToLemmas(hypernymResults.flatten)
      // just get distinct words from the header
      val singleWordsInHeaderLemmatized = reader.convertToLemmas(fullHeader
        .map(_.split("/").tail.mkString("/")) // exclude generic term Event
        .mkString("/") // combine all header lines (multiples possible)
        .split("And|Or") // split on conj
        .mkString("/") // put back together with "/"
        .split("(?=[A-Z])") // split on caps to get words in compounds
        .mkString("/")
        .toLowerCase()
        .split("/").distinct)
//      for (item <- hypernymTermsLemmatized) {
//        println("H: " + item)
//      }
//      for (s <- singleWordsInHeaderLemmatized) println("s: " + s)
      if (singleWordsInHeaderLemmatized.intersect(hypernymTermsLemmatized).nonEmpty) return true else return false
    } else return false


  }
  // get the reader
  val config: Config = ConfigFactory.load()
  val reader = TaxonomyReader.fromConfig

  // create directories and files
  val ontologyDirectory = new File("/media/alexeeva/ee9cacfc-30ac-4859-875f-728f0764925c/storage/lum_related/event-examples-20201129T034043Z-001/event-examples/testNodes")
  val outputDir = new File(ontologyDirectory.getAbsolutePath + "HeaderAndExamplesFilteredWithSimilarityToHeader")
  outputDir.mkdir()
  val files = ontologyDirectory.listFiles()

  for (file <- files) {
    val outfile = new File(outputDir, file.getName.replace(".txt", ".csv"))
//    val cohypOutfile = new File(outputDir, file.getName.replace(".txt", "UpdatedWithCoHyponymsNoLemmas.txt"))
    // retrieve existing examples
    val lines = Source.fromFile(file).getLines().toList
    val (header, examples) = lines.partition(_.startsWith("#"))
    val onlyHeader = false
//    val newHeader = header
//      .map(_.split("/").tail.mkString("/")) // exclude generic term Event
//      .mkString("/") // combine all header lines (multiples possible)
//      .split("And|Or") // split on conj
//      .mkString("/") // put back together with "/"
//      .split("(?=[A-Z])") // split on caps to get words in compounds
//      .mkString(" ") // combine with space and not "/" to keep compounds as one query
//      .toLowerCase()
//      .split("/ ").distinct
    val headerQueries = header.map(_.split("/").last).mkString("/").split("And|Or").mkString("/").split("(?=[A-Z])").mkString(" ").toLowerCase().split("/ ").distinct
    val singleWordsInHeaderLemmatized = reader.convertToLemmas(header
      .map(_.split("/").tail.mkString("/")) // exclude generic term Event
      .mkString("/") // combine all header lines (multiples possible)
      .split("And|Or") // split on conj
      .mkString("/") // put back together with "/"
      .split("(?=[A-Z])") // split on caps to get words in compounds
      .mkString("/")
      .toLowerCase()
      .split("/").distinct)

    val queries = if (onlyHeader) {
      headerQueries
    } else {
//      lines.head.replace("#", "").replace(" ","").split("/") ++ lines.tail
      (headerQueries ++ examples).distinct
    }

    println("header queries: " + headerQueries.mkString("|"))
    println(">>" + queries.mkString("|"))

    val resultsFromAllTerms = new ArrayBuffer[ScoredMatch]()
    val singleWordQueryResult = new ArrayBuffer[ScoredMatch]()
    val multiWordQueryResult = new ArrayBuffer[ScoredMatch]()
//    val cohypResultsFromAllTerms = new ArrayBuffer[String]()

    //have results for overall, for single words, and separate multi-word

    // for every result, return it if it's hypernym is its query, right? or if its hypernym is on the headline list
    // also, dont return if it's already on the list of queries\
    // only output top 10 in th end

    val threshold = 0.75
    for (eg <- queries.filter(_.length > 0).map(_.toLowerCase())) {
//      println("CURRENT EX: " + eg)
      val egAsSeq = eg.split(" ")
      // hyponym results
      val results = reader.getRankedHyponyms(egAsSeq, lemmatize = true)
//      for (r <- results) println(r.query.mkString("|") + " " +  r.query.length + " " + r.score)
      for (r <- results) if (reader.similarityScore(r.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold) resultsFromAllTerms.append(r)
      val (singleWord, multiWord) = results.partition(_.query.length < 2)

      for (sw <- singleWord) if (reader.similarityScore(sw.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold) singleWordQueryResult.append(sw)
      for (mw <- multiWord)  if (reader.similarityScore(mw.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold) multiWordQueryResult.append(mw)
//       co-hyponym results - only do co-hyponyms if the query is deemed relevant
      val coResults = reader.getRankedCohyponyms(egAsSeq, lemmatize = true)
      val (singleWordCohyp, multiWordCohyp) = coResults.partition(_.query.length < 2)
      for (r <-coResults) if (reader.similarityScore(r.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold) resultsFromAllTerms.append(r)
      for (sw <- singleWordCohyp) if (reader.similarityScore(sw.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold)  singleWordQueryResult.append(sw)
      for (mw <- multiWordCohyp) if (reader.similarityScore(mw.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized) > threshold) multiWordQueryResult.append(mw)


//      if (checkIfQueryIsRelevant(results, header)) {
//        println("Query relevant")
//        for (r <- results) resultsFromAllTerms.append(r)
//        val (singleWord, multiWord) = results.partition(_.query.length < 2)
//
//        for (sw <- singleWord) singleWordQueryResult.append(sw)
//        for (mw <- multiWord) multiWordQueryResult.append(mw)
//        // co-hyponym results - only do co-hyponyms if the query is deemed relevant
//        val coResults = reader.getRankedCohyponyms(egAsSeq, lemmatize = true)
//        val (singleWordCohyp, multiWordCohyp) = coResults.partition(_.query.length < 2)
//        for (r <-coResults) resultsFromAllTerms.append(r)
//        for (sw <- singleWordCohyp) singleWordQueryResult.append(sw)
//        for (mw <- multiWordCohyp) multiWordQueryResult.append(mw)
//      } else {
//        println("query not relevant")
//      }


    }


    // filter out results that match queries sort by similarity
    val cleanQueries = queries.filter(_.length > 0).map(_.toLowerCase())
//    println(cleanQueries.mkString("|") + "<<<<")
//    for (r <- resultsFromAllTerms.sortBy(-_.score).slice(0, 10)) {
//      if (cleanQueries.contains(r.result.mkString(" "))) {
//        println("clean queries contains: " + r.result)
//
//      } else {println("clean queries does not contain: " + r.result)}
//    }

      val sortedResults = resultsFromAllTerms.filter(res => !cleanQueries.contains(res.result.mkString(" ").toLowerCase)).sortBy(-_.score).map(res => res.result.mkString(" ") + "\t" + res.query.mkString(" ") + "\t" + res.score.toString + "\t" + res.similarity.toString + "\t" + reader.similarityScore(res.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized).toString + "\n")

      val sortedResultsSingleWord = singleWordQueryResult.filter(res => !cleanQueries.contains(res.result.mkString(" ").toLowerCase)).sortBy(-_.score).map(res => res.result.mkString(" ") + "\t" + res.query.mkString(" ") + "\t" + res.score.toString + "\t" + res.similarity.toString + "\t" + reader.similarityScore(res.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized).toString + "\n")

      val sortedResultsMultiWord = multiWordQueryResult.filter(res => !cleanQueries.contains(res.result.mkString(" ").toLowerCase)).sortBy(-_.score).map(res => res.result.mkString(" ") + "\t" + res.query.mkString(" ") + "\t" + res.score.toString + "\t" + res.similarity.toString + "\t" + reader.similarityScore(res.result.map(_.toLowerCase()), singleWordsInHeaderLemmatized).toString + "\n")


      //    for (r <- sortedResultsSingleWord) println(r)
      //
      val bw = new BufferedWriter(new FileWriter(outfile))

      //    bw.write(lines.head + "\n=======================\n")
      //    // write existing examples to the output file
      //    bw.write(examples.filter(_.length > 0).mkString("\n") + "\n==\n")
      //    // write new examples
      //    bw.write(lines.head + "\n=======================\n")
      //    bw.write("Single- and Multi-word queries:\n")
      //    bw.write(lines.head + "\n------------------------\n")
      bw.write(lines.head)
      bw.write("\n"+examples.filter(_.length > 0).mkString("\n"))
      bw.write("\nresult\tquery\tscore\tsimilarity\tsimilarity_to_header\trelevant\tp@5\tp@10\n")
      bw.write("ALL QUERIES:\n")
      bw.write(sortedResults.distinct.filter(_.length > 0).mkString(""))
      //    bw.write(lines.head + "\n=======================\n")
      //    bw.write("\nSingle-word queries:\n")
      //    bw.write(lines.head + "\n------------------------\n")
      bw.write("SINGLE-WORD QUERIES:\n")
      bw.write(sortedResultsSingleWord.distinct.filter(_.length > 0).mkString(""))
      //    bw.write(lines.head + "\n=======================\n")
      //    bw.write("\nMulti-word queries:\n")
      //    bw.write(lines.head + "\n------------------------\n")
      bw.write("MULTI-WORD QUERIES:\n")
      bw.write(sortedResultsMultiWord.distinct.filter(_.length > 0).mkString(""))
      bw.close()
      ////

////    val chbw = new BufferedWriter(new FileWriter(cohypOutfile))
//    // write existing examples to the output file
//    chbw.write(examples.filter(_.length > 0).mkString("\n") + "\n====\n")
//    // write new examples
//    chbw.write(cohypResultsFromAllTerms.distinct.filter(_.length > 0).mkString("\n"))
//    chbw.close()
  }
}
