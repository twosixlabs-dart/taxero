package ai.lum.eidosdottir.apps

import java.io.File
import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.LazyLogging
import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import ai.lum.common.FileUtils._
import ai.lum.odinson.OdinsonIndexWriter
import ai.lum.odinson.extra.ProcessorsUtils
import ai.lum.odinson.extra.IndexDocuments
import org.clulab.processors.fastnlp.FastNLPProcessor

object IndexCDRs extends App with LazyLogging {

  val config = ConfigFactory.load()
  val proc = new FastNLPProcessor()
  val writer = OdinsonIndexWriter.fromConfig()

  // dir of cdrs
  val cdrDir = config.apply[File]("apps.cdrDir")

  val dataDir = config.apply[String]("odinson.dataDir")
  val textDir = new File(s"$dataDir/text")
  val docsDir = new File(s"$dataDir/docs")

  logger.info("Annotating documents...")
  cdrDir.listFilesByWildcard("*.json").par foreach { cdr =>
    val basename = cdr.getBaseName()
    try {
      val docFilename = s"$docsDir/$basename.json"
      if (!Files.exists(Paths.get(docFilename))) {
        val json = ujson.read(cdr)
        val text = json("extracted_text").str
//        println(text + "<<")
        // Annotate with Processors
        val procDoc = proc.annotate(text)
        // make the OdinsonDoc
        val doc = ProcessorsUtils.convertDocument(procDoc)
        // save text
        val textFile = new File(s"$textDir/$basename.txt")
        textFile.writeString(text)
        // save doc
        val docFile = new File(docFilename)
        docFile.writeString(doc.toJson)
      } else {
        logger.error(s"skipping $basename because $docFilename already exists!")
      }
    } catch {
      case e: StringIndexOutOfBoundsException => logger.error(s"File $basename wasn't processed.")
    }
  }

  val docFiles = docsDir
    .listFilesByWildcards(Seq("*.json", "*.json.gz"), recursive = true)
    .par

  logger.info("Indexing documents")
  IndexDocuments.indexDocuments(writer, docFiles)
  writer.close

}
