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
         
//         val csvSrc = new CsvDataSource(fileDataPath, -1)
     }

     "Gigi algo" should {
         "filtering set-1 by set-2" in new Ctx {

             println("creating SET1...")
             val data1 = data.map(_.direction)

             println("data1 length: " + data1.length)

             val set1 = for(i <- 4 to 13)
                yield Manticore.getDnas(new InlineDataSource(data1), i)
                .map(d => d.map(_._1))


             println("SET1 created which is %d step contains %d strings".format(set1.length,set1.map(_.length).sum))
             println("SET1 details:")

             set1.zipWithIndex.foreach { case (d, i) =>
                 println("  + %d-string = %d patterns".format(i+4, d.length))
             }

             println("creating SET2...")

             val data2: Array[Int] = FractalFinder.extract(data, size)
                 .filter(_.isInstanceOf[Fractal])
                 .map(_.asInstanceOf[Fractal])
                 .map { f =>

                 if (f.pos == FractalPos.TOP_AND_BOTTOM)
                     FractalPos.TOP
                 else
                     f.pos
             }

             println("data2 length: " + data2.length)

             val set2 =
                 for(i <- 4 to 13)
                    yield Manticore.getDnas(new InlineDataSource(data2), i)
                    .map(d => d.map(_._1))

//             println("set2: " + set2)
             println("SET2 created which is %d step %d strings".format(set2.length,set2.map(_.length).sum))
             println("SET2 details:")

             set2.zipWithIndex.foreach { case (d, i) =>
                 println("  + %d-string = %d patterns".format(i+4, d.length))
             }

             println("creating SET3...")
             val set3 = set1.zipWithIndex.map { case (z, i) =>
                 print("set1(" + i + ").length: " + z.length + ", set2(" + i + ").length: " + set2(i).length)
                 var filtered = 0
                 var matched = 0
                 val rv = z.filter { x =>
                     val rv = set2(i).contains(x)
                     if (!rv){
                         filtered = filtered + 1
                     }else{
                         matched = matched + 1
                     }
                     rv
                 }
                 println(", filtered: " + filtered + ", match: " + matched)
                 rv
             }


//             println("set3: " + set3)
             println("SET3 created details: ")
             set3.zipWithIndex.foreach { case (d, i) =>
                 println("   %d-string = %d patterns".format(i + 4, d.length))
             }

             
         }
     }

 }
