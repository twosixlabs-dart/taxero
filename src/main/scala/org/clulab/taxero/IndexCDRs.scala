package ai.lum.taxero

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import ai.lum.common.FileUtils._
import ai.lum.odinson.OdinsonIndexWriter
import ai.lum.odinson.extra.ProcessorsUtils
import ai.lum.odinson.extra.IndexDocuments
import org.clulab.processors.fastnlp.FastNLPProcessor

import scala.io.Source

object IndexCDRs extends App {

  val config = ConfigFactory.load()
  val proc = new FastNLPProcessor()
  val writer = OdinsonIndexWriter.fromConfig()
  val cdrDir = config.apply[File]("apps.cdrDir")

  val dataDir = config.apply[String]("odinson.dataDir")
  val textDir = new File(s"$dataDir/text")
  val docsDir = new File(s"$dataDir/docs")

  val cdrFiles = cdrDir.listFiles()

  for (f <- cdrFiles) {
    val basename = f.getBaseName()
    val json = ujson.read(f)
    val text = json("extracted_text").str
    val procDoc = proc.annotate(text)
    // make the OdinsonDoc
    val doc = ProcessorsUtils.convertDocument(procDoc)
    // save text
    val textFile = new File(s"$textDir/$basename.txt")
    textFile.writeString(text)
      // save doc
     val docFilename = s"$docsDir/$basename.json"
    val docFile = new File(docFilename)
    docFile.writeString(doc.toJson)
  }
  val docFiles = docsDir.listFiles()
  IndexDocuments.indexDocuments(writer, docFiles)
  writer.close


}
