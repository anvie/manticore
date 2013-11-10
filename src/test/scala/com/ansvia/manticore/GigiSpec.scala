package com.ansvia.manticore

import org.specs2.mutable.Specification
import scala.collection.mutable.ArrayBuffer
import java.util.{Date, NoSuchElementException}
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

         val fileDataPath = "/home/robin/EURUSD1b.csv"
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

     "Gigi algo" should {
         "filtering set-1 by set-2" in new Ctx {

             val start = System.currentTimeMillis()

             println("creating SET1...")
             val data1 = data.map(_.direction)

             println("data1 length: " + data1.length)

             val set1 = for(i <- 4 to 13)
                yield Manticore.getDnas(new InlineDataSource(data1), i)
                .map(d => d)


             println("SET1 created which is %d step contains %d strings".format(set1.length,set1.map(_.length).sum))
             println("SET1 details:")

             set1.zipWithIndex.foreach { case (d, i) =>
                 println("  + %d-string = %d patterns".format(i+4, d.length))
             }

             println("creating SET2...")

             val zz = new ZigzagFinder(data)

//             zz.process().getZigZagBuffer.zipWithIndex.foreach { case (z, i) =>
//                 println("%d. %s => %s".format(i, data(i).time, z))
//             }

             val legs = zz.getLegs.filter(leg => leg.fractalCount > 3 && leg.fractalCount < 14)

             var set2: immutable.IndexedSeq[Seq[Seq[Int]]] = immutable.IndexedSeq[Seq[Seq[Int]]]()

             legs.zipWithIndex.foreach { case (d, i) =>
                 println("%d. %s".format(i, d))

                 set2 ++=
                     (for(n <- 4 to 13)
                        yield Manticore.getDnas(new InlineDataSource(d.fractalPattern.map(_.toInt).toSeq), n)
                        .map(dd => dd.map(_._1))).toSeq

//                 val result = dnas.map( dna => Manticore.breakDown(dna, data) )

             }


             val set3 =
                 set1.zipWithIndex.map { case (x, ii) =>
                     x.filter { dna =>
                         val zz = set2(ii)
                         val zz2 = dna.map(_._1)
                         val rv = zz.contains(zz2)
                         rv
                     }
                 }

             set3.zipWithIndex.foreach { case (d, i) =>
                 println(" %d-strings => %s substrings".format(i+4, d.length))
             }
             println("set3.length: " + set3.length)

//
//             val data2: Array[Int] = FractalFinder.find(data, size)
//                 .filter(_.isInstanceOf[Fractal])
//                 .map(_.asInstanceOf[Fractal])
//                 .map { f =>
//
//                 if (f.pos == FractalPos.TOP_AND_BOTTOM)
//                     FractalPos.TOP
//                 else
//                     f.pos
//             }
//
//             println("data2 length: " + data2.length)
//
//             val set2 =
//                 for(i <- 4 to 13)
//                    yield Manticore.getDnas(new InlineDataSource(data2), i)
//                    .map(d => d.map(_._1))
//
//             println("SET2 created which is %d step %d strings".format(set2.length,set2.map(_.length).sum))
//             println("SET2 details:")
//
//             set2.zipWithIndex.foreach { case (d, i) =>
//                 println("  + %d-string = %d patterns".format(i+4, d.length))
//             }
//
//             println("creating SET3...")
//             val set3 = set1.zipWithIndex.map { case (z, i) =>
//                 print("set1(" + (i+4) + ").length: " + z.length + ", set2(" + (i+4) + ").length: " + set2(i).length)
//                 var filtered = 0
//                 var matched = 0
//                 val rv = z.filter { dna =>
////                     println("is set2(i).contains : [" + dna.map(_._1).map(_.toString).mkString(",") + "] ?")
//                     val rv = set2(i).contains(dna.map(_._1))
//                     if (!rv){
//                         filtered = filtered + 1
//                     }else{
//                         matched = matched + 1
//                     }
//                     rv
//                 }
//                 println(", filtered: " + filtered + ", match: " + matched)
//                 rv
//             }
//
//
////             println("set3: " + set3)
//             println("SET3 created. Details: ")
//             set3.zipWithIndex.foreach { case (d, i) =>
//                 println("   %d-string = %d patterns".format(i + 4, d.length))
//             }
//
//             println("Calculating probabilities...")
//             var up = 0
//             var down = 0
//             set3.zipWithIndex.foreach { case (dnas, i) =>
//                 val (positives, negatives, chromosomes) = Manticore.breakDown(dnas, data1)
//                 val probability = if (positives > negatives) {
//                     up = up + 1
//                     "UP"
//                 } else {
//                     down = down + 1
//                     "DOWN"
//                 }
//                 println("  + %d-strings %d chromosomes \t-->\t 1 = %d, 0 = %d, probabilitiy: %s".format(
//                     dnas.head.length,chromosomes, positives, negatives, probability))
//             }
//             println("Done in " + (System.currentTimeMillis() - start) + "ms")
//             println("\n")
//             println("==[ SUMMARY ]==============================================")
//             println("   + up: " + up)
//             println("   + down: " + down)
//             println("\n")

//             println("positive: %d, negatives: %d, chromosomes: %d".format(positive, negatives, chromosomes))
             
         }
     }

 }
