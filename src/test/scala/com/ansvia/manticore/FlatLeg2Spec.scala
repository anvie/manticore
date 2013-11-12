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
class FlatLeg2Spec extends Specification {


     class Ctx extends Scope {

         val fileDataPath = "/home/robin/EURUSD1.csv"
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

     "Flatleg algo" should {
         "calculated correctly" in new Ctx {

             val start = System.currentTimeMillis()

//             println("creating SET1...")
//             val data1 = data.map(_.direction)
//
//             println("data1 length: " + data1.length)
//
//             val set1 = for(i <- 4 to 13)
//                yield Manticore.getDnas(new InlineDataSource(data1), i)
//                .map(d => d)
//
//
//             println("SET1 created which is %d step contains %d strings".format(set1.length,set1.map(_.length).sum))
//             println("SET1 details:")
//
//             set1.zipWithIndex.foreach { case (d, i) =>
//                 println("  + %d-string = %d patterns".format(i+4, d.length))
//             }
//
//             println("creating SET2...")

             val zz = new ZigzagFinder(data)

//             zz.process().getZigZagBuffer.zipWithIndex.foreach { case (z, i) =>
//                 println("%d. %s => %s".format(i, data(i).time, z))
//             }

             val legs = zz.getLegs.toIndexedSeq

//             var set2: immutable.IndexedSeq[Seq[Seq[Int]]] = immutable.IndexedSeq[Seq[Seq[Int]]]()
             var set2a = new mutable.HashMap[Int,Seq[Seq[Int]]]

             legs //.filter(leg => leg.fractalCount > 3 && leg.fractalCount < 14)
                 .zipWithIndex.foreach { case (d, i) =>

                 println("%d. %s".format(i, d))

//                 set2 ++=
                     for(n <- 4 to 13){
                         val aoeu = Manticore.getDnas(new InlineDataSource(d.fractalPattern.map(_.toInt).toSeq), n)
                             .map(dd => dd.map(_._1))
                         if (set2a.contains(n)){
                             val aoeu2 = set2a.get(n).get ++ aoeu
                             set2a.update(n, aoeu2)
                         }else{
                             set2a += n -> aoeu
                         }
                     }


//                 val result = dnas.map( dna => Manticore.breakDown(dna, data) )

             }

             println("")

             var back = 1
             val legUsed = legs(legs.length - back)
             println("last leg: " + legUsed)

             var finalPattern = Seq(legUsed)
             back = back + 1
             var patternLegCount = 1
             val backLegs = 2

//             while(finalPattern.map(_.fractalCount).sum < 4){
             while(finalPattern.length < backLegs){
                 patternLegCount = patternLegCount + 1
                 val leg2 = legs(legs.length - back)
                 finalPattern = Seq(leg2) ++ finalPattern
                 back = back + 1
             }
             
             println("using %d leg(s) as pattern".format(patternLegCount))
             
             val pattBase = finalPattern.map(_.fractalPattern.map(_.toInt).toSeq).toSeq
//             val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
//             val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)

             println("\n")
             println("base pattern: {" + pattBase.map(x => "[" + x.mkString(",") + "]").mkString(",") + "}\n")
//             println("up pattern: {" + pattUp.mkString(",") + "}")
//             println("down pattern: {" + pattDown.mkString(",") + "}")

             var stats = new mutable.HashMap[String, (Int, Leg)]()

             println("Searching for pattern...")
             var patternCount = 0
//             for ( patterns <- set2a.values ){

             val legsCount = legs.length

             var i = 0
             while(i < legsCount - 1){

                 val lSlice = legs.slice(i, i + patternLegCount)
                 var lSlicePatt = lSlice.map(_.fractalPattern.map(_.toInt).toSeq).toSeq

                 if (pattBase == lSlicePatt){
                     println("  + best match at " + legs(i).time)
                     if (i+patternLegCount < legsCount-1){
                         println("    nl: " + legs(i+patternLegCount))
                     }
                 }else{
                     for(pI <- 1 to patternLegCount){
//                         println("i+pI: " + (i + pI) + ", i + (patternLegCount + pI): " + (i + patternLegCount + pI))
                         lSlicePatt = lSlicePatt.slice(pI, patternLegCount).toSeq
//                         if (lSlice.length > 0)
//                            println(lSlice)
                         if (lSlicePatt.length > 0 && lSlicePatt.map(_.length).sum > 2 && pattBase.endsWith(lSlicePatt)){
                             println("  + match at " + legs(i+pI).time + " [" + lSlicePatt.map(x => "{" + x.mkString(",") + "}").mkString(",") + "]")
                             if (i+patternLegCount < legsCount-1){
                                 println("    nl: " + legs(i+patternLegCount))
                             }
                         }
                     }
                 }


                 i = i + 1
             }
//
////                 patterns.foreach { patt =>
//                 legs.zipWithIndex.foreach { case (leg, i) =>
//                     val patt = leg.fractalPattern.toSeq.map(_.toInt)
//                     if (patt/*.map(_._1)*/.startsWith(pattBase)){
//                         val pattStr = patt.mkString(",")
//                         if (patternCount < 20){
//                             println("   + found: {" + pattStr + "}")
//                             if (patternCount == 19)
//                                 println("   + and more...")
//                         }
//                         patternCount = patternCount + 1
//                         val hleg = stats.get(pattStr)
//                         if (hleg.isDefined){
//                             val vv = hleg.get
//                             val count = vv._1 + 1
//                             if (i < legsCount-1){
//                                 stats += pattStr ->  (count, legs(i+1))
//                             }else{
//                                 stats += pattStr -> (count, null)
//                             }
//                         }else{
//                             if (i < legsCount-1){
//                                 stats += pattStr ->  (1, legs(i+1))
//                             }else{
//                                 stats += pattStr -> (1, null)
//                             }
//                         }
//                     }
//                 }
//
////             }
//
//             println("Statistics: ===")
//             for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,10) ){
////                 if (patt != pattBase.mkString(",")){
//                     val count = leg._1
//                     val nextLeg = leg._2
//                     println(" %d \t- %s".format(count, patt))
//                    if (nextLeg != null){
//                        println("   \t   next-leg: " + nextLeg)
//                    }
////                 }
//             }
//
//             println("\n")

         }
     }

 }
