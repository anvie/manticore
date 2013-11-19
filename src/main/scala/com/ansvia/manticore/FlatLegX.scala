package com.ansvia.manticore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import akka.actor.{Props, ActorSystem, Actor}
import akka.routing.RoundRobinRouter
import com.ansvia.manticore.Manticore.{DNAS, DNA}
import java.util.concurrent.CountDownLatch
import scala.collection.immutable.HashMap

/**
 * Author: robin
 * Date: 11/11/13
 * Time: 11:04 PM
 *
 */
object FlatLegX {

    lazy val actorSystem = ActorSystem.create()
    val latch = new CountDownLatch(10)

    case class Calculate(data:IndexedSeq[Int], size:Int, out:ArrayBuffer[DNA])
    case class DnaBarWorker() extends Actor {
        protected def receive = {
            case Calculate(data, size, out) => {
                println("   * [th-%s] %d-strings calculating...".format(Thread.currentThread().getId, size))
                out ++= Manticore.getDnas(new InlineDataSource(data), size)
                latch.countDown()
                println("   * [th-%s] %d-strings done."format(Thread.currentThread().getId, size))
            }
        }
    }
    lazy val workers = actorSystem.actorOf(Props[DnaBarWorker]
        .withRouter(RoundRobinRouter(nrOfInstances=Runtime.getRuntime.availableProcessors())))

    def process(data:IndexedSeq[Record]){

        val dataSize = data.size

//        println("creating SET1...")
//        val data1 = data.map(_.bit)
//
//        println("generating dna bar 4-13 using %d workers...".format(Runtime.getRuntime.availableProcessors()))
//
//        val out = new ArrayBuffer[DNA]
//
//        for(i <- 4 to 13){
//            workers ! Calculate(data1, i, out)
//        }
//
//        println("waiting for workers finished.")
//        latch.await()

//        val set1 = out.result().toSeq


//        println("SET1 created which is %d step contains %d strings".format(set1.length,set1.map(_.length).sum))
//        println("SET1 details:")
//
//        set1.zipWithIndex.foreach { case (d, i) =>
//            println("  + %d-string = %d patterns".format(i+4, d.length))
//        }


        /************************************************
          * Process Zigzag
          ***********************************************/

        val zz = new ZigzagFinder(data, 13, 8, 5)

        val legs = zz.getLegs

        // set2a hanya berisi data pattern dna bar dari legs.
        var set2a = new mutable.HashMap[Int,DNAS]


//        legs //.filter(leg => leg.fractalCount > 3 && leg.fractalCount < 14)
//            .foreach { d =>
//
//            println(d)
//
//            for(n <- 2 to 13){
//                val aoeu = Manticore.getDnas(new InlineDataSource(d.fractalPattern.map(_.toInt).toSeq), n)
////                    .map(dd => dd.map(_._1))
//                if (set2a.contains(n)){
//                    val aoeu2 = set2a.get(n).get ++ aoeu
//                    set2a.update(n, aoeu2)
//                }else{
//                    set2a += n -> aoeu
//                }
//            }
//
//        }


        for(n <- 2 to 14){
            val aoeu = Manticore.getDnas(new InlineDataSource(legs.flatMap(_.fractalPattern.map(_.toInt))), n)
            //                    .map(dd => dd.map(_._1))
            if (set2a.contains(n)){
                val aoeu2 = set2a.get(n).get ++ aoeu
                set2a.update(n, aoeu2)
            }else{
                set2a += n -> aoeu
            }
        }

//        set2a.flatMap(_._2).foreach(x => println(x.map(_._1).mkString("")))

//        val set2b = set1.filter { x =>
//            val p = x.map(_._1)
//            set2a.get(p.length).exists { pp =>
//                pp.contains(p)
//            }
//        }

        println("")

        val lastLeg = legs(legs.length - 1)
//        println("leg used to be pattern: " + legUsed)


        // get uncompleted legs

        val uncompletedLeg =
        {
            //            import scala.util.control.Breaks._
            //            var rv = Seq.newBuilder[Record]

//            val lastLeg = legs(legs.length - 1)

            var trailingData = data.filter(_.timestamp > lastLeg.timestamp)
            var fractals = FractalFinder.find(trailingData)

            if (fractals(fractals.length-1).isInstanceOf[Fractal]){
                trailingData = data.filter(_.timestamp > legs(legs.length - 2).timestamp)
                fractals = FractalFinder.find(trailingData)
            }

            fractals.foreach(x => println("f: " + x))

            val fractalPattern = fractals.filter(_.isInstanceOf[Fractal])
                .map(_.asInstanceOf[Fractal]).map(_.pos)
            val barPattern = trailingData.map(_.bit)
            val fractalCount = fractalPattern.length
            val barCount = barPattern.length

            Leg("-", fractalCount, barCount, fractalPattern.map(_.toByte), barPattern.map(_.toByte).toArray)


            //            rv.result().reverse
            //            uncompletedLeg
        }

        println("uncompletedLeg: " + uncompletedLeg)


//        val trailingBars =
//        {
//            import scala.util.control.Breaks._
//            var rv = Seq.newBuilder[Record]
//            breakable {
//                for (i <- 1 to dataSize - 1){
//                    // searching for last zigzag end point
//                    if (data(dataSize - i).time == lastLeg.time){
//                        break
//                    }else{
//                        rv += data(dataSize - i)
//                    }
//                }
//            }
//            rv.result().reverse
//        }
        val trailingBarPattern = uncompletedLeg.barPattern

        val finalPattern = uncompletedLeg.fractalPattern

        println("trailing bars pattern: {" + trailingBarPattern.map(_.toInt).mkString("") + "}")

        val pattBase: Seq[Int] = finalPattern.map(_.toInt).toSeq
        val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
        val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)

        println("\n")
        println("base fractal pattern: {"+ pattBase.mkString("") + "}")
        println("up fractal pattern: {" + pattUp.mkString("") + "}")
        println("down fractal pattern: {" + pattDown.mkString("") + "}")
        println("\n")


//        if (pattBase.length < 3){
//            throw ManticoreException("insufficient pattern for search.")
//        }


        case class Pattern(str:String, length:Int, occurrences:Int)

        var stats = new mutable.HashMap[String, Pattern]()

        println("Searching for pattern...")


//        def checkPatt(curPatt:Seq[Int], pattToCheck:Seq[Int], leg:Leg) = {
//            if (curPatt == pattToCheck){
//                val pattStr = pattToCheck.map(_.toString).mkString(",")
//
//                val g = stats.get(pattStr)
//                if (g.isDefined){
//                    val count = g.get._2
//                    stats += (pattStr -> (leg.barCount, count + 1))
//                }else{
//                    stats += pattStr -> (leg.barCount, 1)
//                }
//            }
//        }


        legs.zipWithIndex.foreach { case (leg, ii) =>
            val patt = leg.fractalPattern.toSeq.map(_.toInt)

            if (/*patt.length > pattBase.length &&*/ patt.startsWith(pattBase) && patt.length < 9 ){
                val pattStr = patt.map(_.toString).mkString("")

                val g = stats.get(pattStr)
                if (g.isDefined){
                    val count = g.get.occurrences
                    stats += (pattStr -> Pattern(pattStr, leg.barCount, count + 1))
                }else{
                    stats += pattStr -> Pattern(pattStr, leg.barCount, 1)
                }
            }

//            checkPatt(patt, pattBase ++ Seq(1), leg)
//            checkPatt(patt, pattBase ++ Seq(0), leg)
//            checkPatt(patt, pattBase ++ Seq(1,0), leg)
//            checkPatt(patt, pattBase ++ Seq(1,1), leg)
//            checkPatt(patt, pattBase ++ Seq(0,1), leg)
//            checkPatt(patt, pattBase ++ Seq(0,0), leg)
//            checkPatt(patt, pattBase ++ Seq(1,0,1), leg)
//            checkPatt(patt, pattBase ++ Seq(1,0,0), leg)
//            checkPatt(patt, pattBase ++ Seq(1,1,1), leg)
//            checkPatt(patt, pattBase ++ Seq(1,1,0), leg)
//            checkPatt(patt, pattBase ++ Seq(0,0,1), leg)
//            checkPatt(patt, pattBase ++ Seq(0,1,1), leg)
//            checkPatt(patt, pattBase ++ Seq(0,1,0), leg)
//            checkPatt(patt, pattBase ++ Seq(0,0,0), leg)

        }

        if (stats.size == 0)
            throw ManticoreException("No pattern match")

        println("occurrences:")

        val pattBaseStr = pattBase.mkString("")

        if (!stats.get(pattBaseStr).isDefined)
            throw ManticoreException("Base pattern match not found")

        val basePatternOccur = stats(pattBaseStr).occurrences

        println("base pattern occurrences: " + basePatternOccur)

        val sortedStats = stats.toSeq.sortBy(_._2.occurrences).reverse.map(_._2)

//        sortedStats.foreach { patt =>
//            println("%s ->  %d".format(patt.str, patt.occurrences))
//        }

//        val divs = sortedStats.map(p => basePatternOccur / p.occurrences)

        val (ok, bellow) = sortedStats.partition(p => basePatternOccur / p.occurrences > 1)


//        println("ok: " + ok)
//        println("bellow: " + bellow)
        println("ok: %d, bellow: %d, %d/%d > 1 = %d".format(ok.length, bellow.length, ok.length, bellow.length, ok.length/bellow.length))

        if (ok.length == 0)
            throw ManticoreException("exact pattern not found on history, try again later")

        val isPatternMatch = ok.length/bellow.length > 1

        if (!isPatternMatch)
            throw ManticoreException("Pattern not matched yet, try again later")

        println("pattern matched, do the probability calculation...")

        var upCount = 0
        var downCount = 0
        var upBarCount = 0
        var downBarCount = 0

        val pattBar = uncompletedLeg.barPattern.map(_.toInt)
        val pattBarUp = pattBar ++ Seq(1)
        val pattBarDown = pattBar ++ Seq(0)

        println("\n")
        println("base bar pattern: {"+ pattBar.mkString("") + "}")
        println("up bar pattern: {" + pattBarUp.mkString("") + "}")
        println("down bar pattern: {" + pattBarDown.mkString("") + "}")
        println("\n")

        set2a.foreach { case (l, ps) =>
//            ps.foreach { x =>
//                println(x.map(_._1).mkString("") + " == " + pattBarUp.mkString(""))
//            }
            upCount += ps.count(_.map(_._1) == pattUp)
            downCount += ps.count(_.map(_._1) == pattDown)

            upBarCount += ps.count(_.map(_._1) == pattBarUp)
            downBarCount += ps.count(_.map(_._1) == pattBarDown)
        }

        //        val (a,b,c) = Manticore.breakDown(set2a.flatMap(_._2).toSeq, data1, pattBase)

        //        upCount += set2b.count(_.map(_._1) == pattUp)
        //        downCount += set2b.count(_.map(_._1) == pattDown)

        println("FRACTAL: up: " + upCount + ", down: " + downCount)
        println("BAR: up: " + upBarCount + ", down: " + downBarCount)
        //        println("up: " + a + ", down: " + b + ", chroms: " + c)
//
//        println("Statistics: ===")
//        var i = 0
//        for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,5) ){
//            val count = leg._1
//            val nextLegs = leg._3
//            println(" %d \t- %s".format(count, patt))
//
//            if (nextLegs != null){
//                println("   \t   ch-leg: " + leg._2)
//                println("   \t   next-legs (" + nextLegs.length + "): ")
//                val probLegs =
//                    nextLegs.filter(hm => trailingBarPattern.length - hm.barPattern.length <= (trailingBarPattern.length/2)).map { hm =>
//                    //                        val x = hm.barPattern.mkString
//                    //                            var hmm = 0
//                    //                            nextLegs.foreach { hm2 =>
//                    //                            //                            val x2 = hm2.barPattern.mkString
//                    //                                hmm += hammingDistance(hm.barPattern, hm2.barPattern)
//                    //                            }
//
//                        val hmm = hammingDistance(trailingBarPattern, hm.barPattern)
//                        (hm, hmm)
//                    }
//                //                            .sortBy(_._2)
//                //                            .reverse
//                //                            .slice(0, 10)
//
//
//                //                        if (i==0) {
//                var ii = 0
//                probLegs.sortBy(_._2)/*.slice(0,5)*/.foreach {
//                    case (l, power) =>
//                        ii = ii + 1
//                        println("   \t    " + ii + " - " + l + ". power: " + power)
//                }
//                //                        }
//
//                var goodProbLegs:Seq[(Leg, Int)] = Seq.empty[(Leg, Int)]
//
//                if (probLegs.length > 5){
//                    val probI = math.round(probLegs.length / 1.56).toInt
//                    //                            println("probI: " + probI + ", probI/2: " + (probI/2))
//                    println("")
//                    println("   \t   -> good candidate: ")
//                    goodProbLegs = probLegs.sortBy(_._2).slice(probI-2,probI+2)
//                    goodProbLegs.zipWithIndex.foreach { case( goodLeg, ii ) =>
//                        println("   \t    -> (" + (probI-2+ii) + "): " + goodLeg)
//                    }
//                }
//
//                //                    println("   \t   -> cur bpatt prob: {" + getStrongLegBarPattern(nextLegs).map(_.toInt).mkString(",") + "}")
//                println("   \t   -> next leg prob: {" + getProbLeg(nextLegs) + "}")
//
//                val upBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x01)).length
//                val downBin = goodProbLegs.flatMap(_._1.barPattern.filter(_ == 0x00)).length
//                if (upBin > 0 && downBin > 0){
//                    println("   \t Power:")
//                    println("   \t      - UP power: " + upBin + " (" + ( (upBin * 100) / (upBin + downBin) ) + "%)")
//                    println("   \t      - DOWN power: " + downBin + " (" + ( (downBin * 100) / (upBin + downBin) ) + "%)")
//                    println("--------------------------------------------------------------------------------------")
//                }
//
//            }
//
//            i += 1
//        }
    }



    def main(args: Array[String]) {

        val untilDate = {
            if (args.length > 1)
                args(1)
            else
                "-"
        }
        val data = new CsvReader(args(0), untilDate).toArray.toIndexedSeq

        val start = System.currentTimeMillis()

        try {
            process(data)
        }catch{
            case e:ManticoreException =>
                System.err.println(e.getMessage)
        }finally{
            actorSystem.shutdown()
            NonBlockingChromosomeFinder.system.shutdown()
        }

        println("Done in " + (System.currentTimeMillis() - start) + "ms")
        println("\n")

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
            if (bp.filter(_ == 0x01).length > bp.filter(_ == 0x00).length){
                buff += 0x01
            }else{
                buff += 0x00
            }
            i = i + 1
        }

        Leg("xxx", numFracAvg, numBarAvg, Array.empty[Byte], buff.result().toArray)

    }

    // Calculate a sum of set bits of XOR'ed bytes
    def hammingDistance(b1: Array[Byte], b2: Array[Byte]) = {
        (b1.zip(b2).map((x: (Byte, Byte)) => numberOfBitsSet((x._1 ^ x._2).toByte))).sum
    }

    // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
    def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum
}
