package com.ansvia.manticore

import org.specs2.mutable.Specification
import scala.collection.mutable.ArrayBuffer
import java.util.NoSuchElementException
import scala.collection.immutable
import org.specs2.specification.Scope

/**
  * Author: robin
  * Date: 11/9/13
  * Time: 11:28 AM
  *
  */
class GigiSpec extends Specification {


     class Ctx extends Scope {

         val fileDataPath = "data/EURUSD1.csv"
         
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
         
         val csvSrc = new CsvDataSource(fileDataPath, -1)
     }

     "Gigi algo" should {
         "filtering set-1 by set-2" in new Ctx {

             println("creating SET1...")
             val set1 = for(i <- 4 to 13)
                yield Manticore.getDnas(csvSrc, i)
                .map(d => d.map(_._1))

             val data2: Array[Int] = FractalFinder.extract(data, size)
                 .filter(_.isInstanceOf[Fractal]).map(_.asInstanceOf[Fractal])
                 .map { f =>

                 if (f.pos == FractalPos.TOP_AND_BOTTOM)
                     FractalPos.TOP
                 else
                     f.pos
             }
             println("SET1 created which is contains %d dnas".format(set1.map(_.length).sum))
             println("creating SET2...")
             val set2 =
                 (for(i <- 4 to 13) yield i).flatMap( i => Manticore.getDnas(new InlineDataSource(data2), i))
                     .map(d => d.map(_._1))

//             println("set2: " + set2)
             println("SET2 created which is contains %d dnas".format(set2.map(_.length).sum))

             println("creating SET3...")
             val set3 = set1.map(_.filter(x => set2.contains(x)))


             println("set3: " + set3)
             println("SET3 created details: ")
             set3.zipWithIndex.foreach { case (d, i) =>
                 println("   %d-string = %d patterns".format(i + 4, d.length))
             }

             
         }
     }

 }
