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
class FlatLegXSpec extends Specification {



    class Ctx extends Scope {

        val fileDataPath = "/home/robin/Downloads/EURUSD5.csv"
//        val fileDataPath = "/home/robin/Downloads/EURUSD_Candlestick_1_D_BID_01.01.2007-09.11.2013.csv"
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
        val dataSize = data.size
        //        val data = data.reverse
        //        val data = data
        val size:Int = data.size

        csvReader.close()

        //         val csvSrc = new CsvDataSource(fileDataPath, -1)
    }

    def getProbLeg(legs:Seq[Leg]) = {

        var buff = new ArrayBuffer[Byte]
        val numFracAvg = legs.map(_.fractalCount).sum / legs.length
        val numBarAvg = legs.map(_.barCount).sum / legs.length

        var i = 0
        while(i < numBarAvg){
            val bp = legs.map { x =>
                if (i < x.barPattern.length) {
                    x.barPattern(i)
                } else {
                    0xFF
                }
            }
            if (bp.count(_ == 0x01) > bp.count(_ == 0x00)){
                buff += 0x01
            }else{
                buff += 0x00
            }
            i = i + 1
        }

        Leg("xxx", numFracAvg, numBarAvg, Array.empty[Byte], buff.result().toArray)

    }


    "Gigi algo" should {
        "filtering set-1 by set-2" in new Ctx {

            val start = System.currentTimeMillis()

            val zz = new ZigzagFinder(data)

            val legs = zz.getLegs

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

            }

            println("")

            var back = 1
            val legUsed = legs(legs.length - back)
            println("leg used to be pattern: " + legUsed)

            
            val trailingBars =
            {
                import scala.util.control.Breaks._
                var rv = Seq.newBuilder[Record]
                breakable {
                    for (i <- 1 to dataSize - 1){
                        // searching for last zigzag end point
                        if (data(dataSize - i).time == legUsed.time){
                            break
                        }else{
                            rv += data(dataSize - i)
                        }
                    }
                }
                rv.result().reverse
            }
            val trailingBarPattern = trailingBars.map(_.direction.toByte).toArray

            var finalPattern = legUsed.fractalPattern
            back = back + 1
            var legCount = 1
//            while(finalPattern.length < 3){
//                legCount = legCount + 1
//                val leg2 = legs(legs.length - back)
//                finalPattern = leg2.fractalPattern ++ finalPattern
//                back = back + 1
//            }

            println("using %d leg(s) as pattern".format(legCount))
            println("trailing bars pattern: {" + trailingBarPattern.map(_.toInt).mkString(",") + "}")

            val pattBase: Seq[Int] = finalPattern.map(_.toInt).toSeq
            val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
            val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)

            println("\n")
            println("base pattern: {"+ pattBase.mkString(",") + "}\n")
            println("up pattern: {" + pattUp.mkString(",") + "}")
            println("down pattern: {" + pattDown.mkString(",") + "}")

            // (Occurences Count, Current history leg?, Next leg?)
            var stats = new mutable.HashMap[String, (Int, Seq[Leg], Seq[Leg])]()

            println("Searching for pattern...")
            var patternCount = 0
            //             for ( patterns <- set2a.values ){

            val legsCount = legs.length

            //                 patterns.foreach { patt =>
            legs.zipWithIndex.foreach { case (leg, ii) =>
                val patt = leg.fractalPattern.toSeq.map(_.toInt)

//                val threshold = (leg.barCount - legUsed.barCount)
                if (patt/*.map(_._1)*/.startsWith(pattBase) &&
                    (leg.barCount < (legUsed.barCount + 3)) &&
//                    ((leg.barCount - legUsed.barCount) < 10) &&
                    (leg.fractalCount < (legUsed.fractalCount + 5)) &&
                    (leg.fractalCount > legUsed.fractalCount) ){

                    val pattStr = patt.mkString(",")
                    if (patternCount < 20){
                        println("   + found: {" + pattStr + "}")
                        if (patternCount == 19)
                            println("   + and more...")
                    }
                    patternCount = patternCount + 1
                    val hleg = stats.get(pattStr)
                    if (hleg.isDefined){
                        val vv = hleg.get
                        val count = vv._1 + 1
                        val curLegs = vv._2 ++ Seq(leg)
                        if (ii < legsCount-1){
                            val nextLegs = vv._3 ++ Seq(legs(ii+1))
                            stats += pattStr ->  (count, curLegs, nextLegs)
                        }else{
                            stats += pattStr -> (count, curLegs, null)
                        }
                    }else{
                        if (ii < legsCount-1){
                            stats += pattStr -> (1, Seq(leg), Seq(legs(ii+1)))
                        }else{
                            stats += pattStr -> (1, Seq(leg), null)
                        }
                    }
                }
            }

            //             }

            println("Statistics: ===")
            var i = 0
            for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,5) ){
                val count = leg._1
                val nextLegs = leg._3
                println(" %d \t- %s".format(count, patt))

                if (nextLegs != null){
                    val chLegProb = getProbLeg(leg._2)
                    println("   \t   ch-leg-prob: " + chLegProb)
                    println("   \t   next-legs (" + nextLegs.length + "): ")
                    val probLegs =
                    nextLegs.filter(hm => trailingBarPattern.length - hm.barPattern.length <= (trailingBarPattern.length/2)).map { hm =>
                    //                        val x = hm.barPattern.mkString
//                            var hmm = 0
//                            nextLegs.foreach { hm2 =>
//                            //                            val x2 = hm2.barPattern.mkString
//                                hmm += hammingDistance(hm.barPattern, hm2.barPattern)
//                            }

                        val hmm = hammingDistance(trailingBarPattern, hm.barPattern)
                        (hm, hmm)
                    }
//                            .sortBy(_._2)
//                            .reverse
//                            .slice(0, 10)


//                        if (i==0) {
                        var ii = 0
                        probLegs.sortBy(_._2).slice(0,5).foreach {
                            case (l, power) =>
                                ii = ii + 1
                                println("   \t    " + ii + " - " + l + ". power: " + power)
                        }
//                        }

                    var goodProbLegs:Seq[(Leg, Int)] = Seq.empty[(Leg, Int)]

                    if (probLegs.length > 5){
                        val probI = math.round(probLegs.length / 1.56).toInt
//                            println("probI: " + probI + ", probI/2: " + (probI/2))
                        println("")
                        println("   \t   -> good candidate: ")
                        goodProbLegs = probLegs.sortBy(_._2).slice(probI-2,probI+2)
                        goodProbLegs.zipWithIndex.foreach { case( goodLeg, ii ) =>
                            println("   \t    -> (" + (probI-2+ii) + "): " + goodLeg)
                        }
                    }

//                    println("   \t   -> cur bpatt prob: {" + getStrongLegBarPattern(nextLegs).map(_.toInt).mkString(",") + "}")
                    val ll = goodProbLegs.map(_._1)
                    if (ll.length > 1){
                        println("   \t   -> next leg prob: {" + getProbLeg(ll) + "}")
                    }

                    val upBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x01)).length
                    val downBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x00)).length
                    if (upBin > 0 && downBin > 0){
                        println("   \t Power:")
                        println("   \t      - UP power: " + upBin + " (" + ( (upBin * 100) / (upBin + downBin) ) + "%)")
                        println("   \t      - DOWN power: " + downBin + " (" + ( (downBin * 100) / (upBin + downBin) ) + "%)")
                        println("--------------------------------------------------------------------------------------")
                    }

                }

                i += 1
            }

            println("\n")


        }
    }

    // Calculate a sum of set bits of XOR'ed bytes
    def hammingDistance(b1: Array[Byte], b2: Array[Byte]) = {
        (b1.zip(b2).map((x: (Byte, Byte)) => numberOfBitsSet((x._1 ^ x._2).toByte))).sum
    }

    // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
    def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum
}
