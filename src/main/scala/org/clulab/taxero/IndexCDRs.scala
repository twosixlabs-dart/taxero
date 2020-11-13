package ai.lum.taxero

import java.io.File
import java.nio.file.{Files, Paths}

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
  val cdrDir = config.apply[File]("apps.cdrDir")

  val dataDir = config.apply[String]("odinson.dataDir")
  val textDir = new File(s"$dataDir/text")
  val docsDir = new File(s"$dataDir/docs")

  val cdrFiles = cdrDir.listFiles()

  logger.info("Annotating documents...")
  for (f <- cdrFiles) {
    val basename = f.getBaseName()
    val docFilename = s"$docsDir/$basename.json"
    val json = ujson.read(f)
    if (!Files.exists(Paths.get(docFilename))) {
        val text = json.obj.getOrElse("extracted_text", "").toString
        // Annotate with Processors
      if (text.length > 0) {
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
        logger.error(s"skipping $basename because file may be missing the `extracted_text` field")
      }

    } else {
      logger.error(s"skipping $basename because $docFilename already exists!")
    }
  }

  logger.info("Indexing documents")
  val docFiles = docsDir.listFiles()
  IndexDocuments.indexDocuments(writer, docFiles)
  writer.close

}
