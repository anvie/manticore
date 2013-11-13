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

            val legs = zz.getLegs

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
            println("leg used to be pattern: " + legUsed)

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

            val pattBase: Seq[Int] = finalPattern.map(_.toInt).toSeq
            val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
            val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)

            println("\n")
            println("base pattern: {"+ pattBase.mkString(",") + "}\n")
            println("up pattern: {" + pattUp.mkString(",") + "}")
            println("down pattern: {" + pattDown.mkString(",") + "}")

            // (Occurences Count, Current history leg?, Next leg?)
            var stats = new mutable.HashMap[String, (Int, Leg, Seq[Leg])]()

            println("Searching for pattern...")
            var patternCount = 0
            //             for ( patterns <- set2a.values ){

            val legsCount = legs.length

            //                 patterns.foreach { patt =>
            legs.zipWithIndex.foreach { case (leg, ii) =>
                val patt = leg.fractalPattern.toSeq.map(_.toInt)

//                val threshold = (leg.barCount - legUsed.barCount)
                if (patt/*.map(_._1)*/.startsWith(pattBase) &&
                    (leg.barCount >= legUsed.barCount) &&
                    ((leg.barCount - legUsed.barCount) < 10) &&
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
                        if (ii < legsCount-1){
                            val nextLegs = vv._3 ++ Seq(legs(ii+1))
                            stats += pattStr ->  (count, leg, nextLegs)
                        }else{
                            stats += pattStr -> (count, leg, null)
                        }
                    }else{
                        if (ii < legsCount-1){
                            stats += pattStr -> (1, leg, Seq(legs(ii+1)))
                        }else{
                            stats += pattStr -> (1, leg, null)
                        }
                    }
                }
            }

            //             }

            println("Statistics: ===")
            var i = 0
            for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,10) ){
                //                 if (patt != pattBase.mkString(",")){
                val count = leg._1
                val nextLegs = leg._3
                println(" %d \t- %s".format(count, patt))

//                if (i == 0){
                    if (nextLegs != null){
                        println("   \t   ch-leg: " + leg._2)
                        println("   \t   next-legs (" + nextLegs.length + "): ")
                        val probLegs =
                        nextLegs.map { hm =>
                        //                        val x = hm.barPattern.mkString
                            var hmm = 0
                            nextLegs.foreach { hm2 =>
                            //                            val x2 = hm2.barPattern.mkString
                                hmm += hammingDistance(hm.barPattern, hm2.barPattern)
                            }
                            (hm, hmm)
                        }
//                            .sortBy(_._2)
//                            .reverse
//                            .slice(0, 10)


                        probLegs.slice(0,5).foreach { case (l, power) =>
                            println("   \t   - " + l + ". power: " + power)
                        }

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
                        val upBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x01)).length
//                        println(probLegs.flatMap(_._1.barPattern.filter(_ == 0x01)))
                        val downBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x00)).length
                        println("   \t Power:")
                        println("   \t      - UP power: " + upBin + " (" + ( (upBin * 100) / (upBin + downBin) ) + "%)")
                        println("   \t      - DOWN power: " + downBin + " (" + ( (downBin * 100) / (upBin + downBin) ) + "%)")
                        println("--------------------------------------------------------------------------------------")

                    }

//                }
                i += 1

                //                 }
            }

            println("\n")
            //
            //             val set3 =
            //                 set1.zipWithIndex.map { case (x, ii) =>
            //                     x.filter { dna =>
            //                         val zz = set2a(ii+4)
            //                         val zz2 = dna.map(_._1)
            //                         val rv = zz.contains(zz2)
            //                         rv
            //                     }
            //                 }
            //
            //             var up = 0
            //             var down = 0
            //             val patternPositive = data.slice(data.length-5, data.length-1).map(_.direction) ++ Seq(1)
            //             val patternNegative = data.slice(data.length-5, data.length-1).map(_.direction) ++ Seq(0)
            //
            //             println("looking for pattern: +{" + patternPositive.mkString(",") + "} -{" + patternNegative.mkString(",") + "}")
            //
            //
            //             set3.zipWithIndex.foreach { case (d, i) =>
            ////                 println(" %d-strings => %s substrings".format(i+4, d.length))
            //
            //                 d.foreach {
            //                     dd =>
            ////                         println("{" + dd.map( xx => xx._1.toString /*"%s(%s)".format(xx._1,xx._2)*/ ).mkString(",") + "}")
            //
            //                         if (dd.map(_._1) == patternPositive)
            //                             up += 1
            //
            //                         if (dd.map(_._1) == patternNegative)
            //                             down += 1
            //                 }
            //             }
            //
            //             println("result: ====")
            //             println("  up: " + up)
            //             println("  down: " + down)














            //             println("set3.length: " + set3.length)

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

    // Calculate a sum of set bits of XOR'ed bytes
    def hammingDistance(b1: Array[Byte], b2: Array[Byte]) = {
        (b1.zip(b2).map((x: (Byte, Byte)) => numberOfBitsSet((x._1 ^ x._2).toByte))).sum
    }

    // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
    def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum
}
