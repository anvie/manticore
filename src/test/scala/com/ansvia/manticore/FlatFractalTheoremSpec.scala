package com.ansvia.manticore

import org.specs2.mutable.Specification
import scala.collection.mutable.ArrayBuffer
import java.util.NoSuchElementException
import scala.collection.{mutable, immutable}
import org.specs2.specification.Scope

/**
  * Author: robin
  * Date: 11/9/13
  * Time: 11:28 AM
  *
  */
class FlatFractalTheoremSpec extends Specification {


     class Ctx extends Scope {

         val fileDataPath = "/home/robin/EURUSD5.csv"
//         val fileDataPath = "/home/aoeu/EURUSD240.csv"

         val csvReader = new CsvReader(fileDataPath)

         println(" + data source: " + fileDataPath)

         println(" + loading data into memory...")

         var buff = new ArrayBuffer[Record]()

         var rec:Record = null
         try {
             while (true) {
                 rec = csvReader.nextRecord()
                 //                println("  high: " + rec.high + ", low: " + rec.low)
                 buff :+= rec
             }
         }
         catch {
             case e:NoSuchElementException =>
         }

         val data:immutable.IndexedSeq[Record] = buff.result().toIndexedSeq
         //        val data = data.reverse
         //        val data = data
         val size:Int = data.size

         csvReader.close()
         
//         val csvSrc = new CsvDataSource(fileDataPath, -1)
     }

     "FlatFractal algo" should {
         "calculating as expected" in new Ctx {

             val start = System.currentTimeMillis()


             val fractals = FractalFinder.find(data, data.size)

             val fractalsBinary = fractals.filter(_.isInstanceOf[Fractal])
                 .map(_.asInstanceOf[Fractal])
                 .map(_.pos)
                 .toSeq

//             println(fractalsBinary.mkString(","))

//             val strings = for(i <- 4 to 13)
                 //yield Manticore.getDnas(new InlineDataSource(fractalsBinary), i)
             val string = Manticore.getDnas(new InlineDataSource(fractalsBinary), 13)

//             for (string <- strings){
                 val (positives, negatives, chroms) = Manticore.breakDown(string, fractalsBinary.toIndexedSeq)

                 println("positives: %d, negatives: %d, chroms: %d".format(positives, negatives, chroms))
                 val nextFractal = if (positives > negatives) "up" else "down"
                 println("next fractal will be " + nextFractal)
//             }
         }
     }

 }
