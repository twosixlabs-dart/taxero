package org.clulab.taxero
import java.io.File
import ai.lum.common.FileUtils._
import scala.io.Source
import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._

import scala.collection.immutable.Stream.continually
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object GenerateSampleEval extends App {
  val config = ConfigFactory.load()
  println("started reading the code...")
  
  val evalfilenamepath: String = config[String]("eval.files.path")
  val evalsamplefile = config[File]("sample.eval.output")
  val evalnumfile = config[File]("number.of.results.output")
  val file = new File("evalfilenamepath")
  println("reading the directory")
  println(file)
  // val files = file.listFiles()
  //to exclude directories 
  val files = file.listFiles().filter(_.isFile)
  println("looped through directory")
  for (i <- files) {
   println(i)
  }
  // println(files)

  for (f <- files) {
    val oneCountlist = new ArrayBuffer[String]()
    val moreThanOneCountlist = new ArrayBuffer[String]()

    val numlines: Int = Source.fromFile(f).getLines.size
    println("number of lines: " + numlines)
    evalnumfile.writeString(s"${f.getName}\t$numlines\n", append = true)
    evalsamplefile.writeString(s"${f.getName}\n", append = true)

    for (line <- Source.fromFile(f).getLines()) {
      val tokens = line.trim.split("\t")
      if (tokens(2).toInt == 1) {
        oneCountlist += line
      } else {
        moreThanOneCountlist += line
      }
    }
      // assume len(oneCountlist) and len(moreThanOneCountlist) each are > 250 ??
    if (numlines > 500) {
//       val r = new util.Random
//       val randomlist_1 = continually(r.nextInt(oneCountlist.size)).take(250).toList
//       val randomlist_2 = continually(r.nextInt(moreThanOneCountlist.size)).take(250).toList

      val r = scala.util.Random
      var temp_1:Int = 0
      var temp_2:Int = 0
      var randomSet_1:Set[Int] = Set()
      var randomSet_2:Set[Int] = Set()

      var i_1:Int = 0
      while(i_1<250){
        temp_1 = r.nextInt(oneCountlist.size)
        if(!randomSet_1.contains(temp_1)){
          randomSet_1=randomSet_1+temp_1
          i_1+=1
        }
      }
      val randomlist_1 = randomSet_1.toList

      var i_2:Int = 0
      while(i_2<250){
        temp_2 = r.nextInt(moreThanOneCountlist.size)
        if(!randomSet_2.contains(temp_2)){
          randomSet_2=randomSet_2+temp_2
          i_2+=1
        }
      }
      val randomlist_2 = randomSet_2.toList


      for (num <- randomlist_1) {
        evalsamplefile.writeString(s"${oneCountlist(num)}\n", append = true)
      }
      for (num <- randomlist_2) {
        evalsamplefile.writeString(s"${moreThanOneCountlist(num)}\n", append = true)
      }

    } else {
      for (line <- Source.fromFile(f).getLines) {
        evalsamplefile.writeString(s"$line\n", append = true)
      }
    }

  }

}