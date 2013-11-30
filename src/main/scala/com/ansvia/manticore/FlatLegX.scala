package com.ansvia.manticore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import akka.actor.{Props, ActorSystem, Actor}
import akka.routing.RoundRobinRouter
import com.ansvia.manticore.Manticore.{DNAS, DNA}
import java.util.concurrent.CountDownLatch
import scala.collection.immutable.HashMap
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric

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

    def process(data:IndexedSeq[Record], usingPatternMatch:Boolean){

        val dataSize = data.size

        println("data size: " + dataSize)

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

        println("calculating zigzag...")

        val zz = new ZigzagFinder(data, 13, 8, 5)

        val legs = zz.getLegs

        println(legs.length + " zigzag legs loaded")

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
            if (set2a.contains(n)){
                val aoeu2 = set2a.get(n).get ++ aoeu
                set2a.update(n, aoeu2)
            } else {
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

        println("last leg: " + lastLeg)


        // get uncompleted leg

        val uncompletedLeg =
        {

            val trailingData = data.filter(_.timestamp > lastLeg.timestamp)
            var fractals = FractalFinder.find(trailingData)

            if (fractals(fractals.length-1).isInstanceOf[Fractal]){
                fractals = fractals.slice(0, fractals.length-2)
            }

//            fractals.foreach(x => println("f: " + x))

            val fractalPattern = fractals.filter(_.isInstanceOf[Fractal])
                .map(_.asInstanceOf[Fractal]).map(_.pos)
            val barPattern = trailingData.map(_.bit)
            val fractalCount = fractalPattern.length
            val barCount = barPattern.length

            if (fractalCount == 0 && barCount < 2){
                // using last leg to be uncompleted leg

                println("uncompleted leg too sort, using last leg as the uncompleted leg pattern")

                lastLeg
            }else{
                Leg("-", fractalCount, barCount, fractalPattern.map(_.toByte), barPattern.map(_.toByte).toArray, 0.0)
            }

        }

        println("uncompleted leg: " + uncompletedLeg)

        val trailingBarPattern = uncompletedLeg.barPattern

        val finalPattern = uncompletedLeg.fractalPattern

        println("trailing bars pattern: {" + trailingBarPattern.map(_.toInt).mkString("") + "}")

        // diambil dari fractal pattern.
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

        }

        if (usingPatternMatch){

            //// BEGIN PROBABILITY STYLE PATTERN MATCHING ----------------
//            println("using pattern matching, check for pattern match.")
//
//            println("Searching for pattern...")
//
//            if (stats.size == 0)
//                throw ManticoreException("No pattern match")
//
//            println("occurrences:")
//
//            val pattBaseStr = pattBase.mkString("")
//
//            if (!stats.get(pattBaseStr).isDefined)
//                throw ManticoreException("Base pattern match not found")
//
//            val basePatternOccur = stats(pattBaseStr).occurrences
//
//            println("base pattern occurrences: " + basePatternOccur)
//
//            val sortedStats = stats.toSeq.sortBy(_._2.occurrences).reverse.map(_._2)
//
//            val (ok, bellow) = sortedStats.partition(p => basePatternOccur / p.occurrences > 1)
//
//
//            println("ok: %d, bellow: %d, %d/%d > 1 = %d".format(ok.length, bellow.length,
//                ok.length, bellow.length, ok.length/bellow.length))
//
//            if (ok.length == 0)
//                throw ManticoreException("exact pattern not found on history, try again later")
//
//            val isPatternMatch = ok.length/bellow.length > 1
            //// END OF PROBABILITY STYLE PATTERN MATCHING ----------------

            val isPatternMatch = legs.exists(l => l.similarity() == uncompletedLeg.similarity())

            if (!isPatternMatch)
                throw ManticoreException("Pattern not matched yet, try again later")

            println("pattern matched, do the probability calculation...")
        }else{
            println("pattern matching skiped.")
        }

        var upCount = 0
        var downCount = 0
        var upBarCount = 0
        var downBarCount = 0
        var allAlgoResults = Seq.newBuilder[Int]

        // diambil dari candle pattern
        val pattBar = uncompletedLeg.barPattern.map(_.toInt)
        var pattBarSeq = Seq.empty[Seq[Int]]
        if (pattBar.length >= 4){
//            pattBar = pattBar.reverse.slice(0, 7).reverse
            for (ii <- 4 to pattBar.length){
                pattBarSeq :+= pattBar.reverse.slice(0, ii).reverse.toSeq
            }
        }
        val pattBarUp = pattBar ++ Seq(1)
        val pattBarDown = pattBar ++ Seq(0)

        println("\n")
        println("base bar pattern: {"+ pattBar.mkString("") + "}")
        pattBarSeq.zipWithIndex.foreach { case (p, i) =>
            println("   %d-strings: %s".format(i+4, p.mkString("")))
        }
        println("up bar pattern: {" + pattBarUp.mkString("") + "}")
        println("down bar pattern: {" + pattBarDown.mkString("") + "}")
        println("\n")

        set2a.foreach { case (l, ps) =>
//            ps.foreach { x =>
//                if (x.map(_._1).length == pattBarUp.length)
//                    println(x.map(_._1).mkString("") + " == " + pattBarUp.mkString(""))
//            }
            upCount += ps.count(_.map(_._1) == pattUp)
            downCount += ps.count(_.map(_._1) == pattDown)

            pattBarSeq.zipWithIndex.foreach { case (p, i)=>
               upBarCount += ps.count(_.map(_._1).mkString("") == (p ++ Seq(1)).mkString(""))
               downBarCount += ps.count(_.map(_._1).mkString("") == (p ++ Seq(0)).mkString(""))
            }
        }

        allAlgoResults.+=(if (upCount > downCount) 1 else if (upCount < downCount) 0 else -1)
        allAlgoResults.+=(if (upBarCount > downBarCount) 1 else if (upBarCount < downBarCount) 0 else -1)

//
//        println("Calculating sub dna...")
//        var legBin = set2a.flatMap(_._2).toSeq
//        if (legBin.length > 1024){
//            legBin = legBin.slice(0, 1024)
//        }
//        val (fsDnaUp,fsDnaDown,fsDnaOccurs) = Manticore.breakDown(legBin, legBin.flatMap(_.map(_._1)).toIndexedSeq, pattBar)

//        legs.flatMap(_.fractalPattern).map(_.toInt).mkString("")

//        val rawDataBit = data.map(_.bit)
//        val candleDnaBar = Manticore.getDnas(new InlineDataSource(rawDataBit), 7)
//        val (cdDnaUp,cdDnaDown,cdDnaOccurs) = Manticore.breakDown(candleDnaBar, rawDataBit)
//        val (cdfDnaUp,cdfDnaDown,cdfDnaOccurs) = Manticore.breakDown(candleDnaBar, legBin.flatMap(_.map(_._1)).toIndexedSeq)

        //        upCount += set2b.count(_.map(_._1) == pattUp)
        //        downCount += set2b.count(_.map(_._1) == pattDown)

        println("FRACTAL: up: " + upCount + ", down: " + downCount + " --> " + (if (upCount>downCount) "UP" else if (upCount==downCount) "-" else "DOWN"))
        println("LEG BAR: up: " + upBarCount + ", down: " + downBarCount + " --> " + (if (upBarCount>downBarCount) "UP" else if (upBarCount==downBarCount) "-" else "DOWN"))
//        println("FRACTAL SUB DNA: up: " + fsDnaUp + ", down: " + fsDnaDown + " --> " + (if (fsDnaUp>fsDnaDown) "UP" else if (fsDnaUp==fsDnaDown) "-" else "DOWN"))
//        println("CANDLE SUB DNA (raw): up: " + cdDnaUp + ", down: " + cdDnaDown + " --> " + (if (cdDnaUp>cdDnaDown) "UP" else if (cdDnaUp==cdDnaDown) "-" else "DOWN"))
//        println("CANDLE SUB DNA (filtered): up: " + cdfDnaUp + ", down: " + cdfDnaDown + " --> " + (if (cdfDnaUp>cdfDnaDown) "UP" else if (cdfDnaUp==cdfDnaDown) "-" else "DOWN"))


        {
            /**
             * Using leg matching algo (heur-1)
             */
            val matchedLegs = legs.filter { leg =>
                leg.fractalPattern.startsWith(uncompletedLeg.fractalPattern) &&
                    leg.barCount < (uncompletedLeg.barCount+3) //&&
//                    (uncompletedLeg.position=="-" || leg.position == uncompletedLeg.position)
            }
            //        println("matched legs:")
            //        matchedLegs.foreach(println)

            val matchedLegsStats = matchedLegs.groupBy(_.similarity()).filter(_._2.length > 3)
//            matchedLegsStats.toSeq.sortBy(_._2.length)
//                .reverse.foreach { case (hash, _legs) =>
//
//                println("%d. %s".format(_legs.length, _legs(0)))
//
//                _legs.groupBy(_.barPattern.mkString("")).toSeq.sortBy(_._2.length).reverse
//                    .foreach { case (bpat, _legs2) =>
//
//                    println("    %s - %s".format(bpat, _legs2.length))
//                }
//            }

//            val x = matchedLegsStats.flatMap(_._2)
//            println("    pattern: " + matchedLegsStats.flatMap(_._2).map(_.position).mkString(" "))
            val (down, up) = matchedLegsStats.flatMap(_._2).map(_.position).partition(_ == "up")
            val state = {
                val a = down.size
                val b = up.size

                if (a == 0 && b > 0) "UP"
                else if (b == 0 && a > 0) "DOWN"
                else "-"
    
            }

            println("MANTICORE HEUR-1 (%d/%d) --> %s".format(up.size, down.size, state))

            allAlgoResults.+=(if (state == "UP") 1 else if (state == "DOWN") 0 else -1)
        }

        {
            /**
             * Using leg matching algo (heur-2) menggunakan hamming distance
             */
            val matchedLegs = legs.filter { leg =>
                leg.fractalPattern.startsWith(uncompletedLeg.fractalPattern) &&
                    leg.barCount < (uncompletedLeg.barCount+3) //&&
//                    (uncompletedLeg.position=="-" || leg.position == uncompletedLeg.position)
            }
            //        println("matched legs:")
            //        matchedLegs.foreach(println)

            val matchedLegsStats = matchedLegs.groupBy { l =>
                hammingDistance(l.barPattern, uncompletedLeg.barPattern)
            }.filter(_._2.length > 3)
//            matchedLegsStats.toSeq.sortBy(_._2.length)
//                .reverse.foreach { case (hash, _legs) =>
//
//                println("%d. %s".format(_legs.length, _legs(0)))
//
//                _legs.groupBy(_.barPattern.mkString("")).toSeq.sortBy(_._2.length).reverse
//                    .foreach { case (bpat, _legs2) =>
//
//                    println("    %s - %s".format(bpat, _legs2.length))
//                }
//            }

//            val x = matchedLegsStats.flatMap(_._2)
//            println("    pattern: " + matchedLegsStats.flatMap(_._2).map(_.position).mkString(" "))
            val (down, up) = matchedLegsStats.flatMap(_._2).map(_.position).partition(_ == "up")
            val state = {
                val a = down.size
                val b = up.size

                if (a == 0 && b > 0) "UP"
                else if (b == 0 && a > 0) "DOWN"
                else "-"

            }

            println("MANTICORE HEUR-2 (%d/%d) --> %s".format(up.size, down.size, state))

            allAlgoResults.+=(if (state == "UP") 1 else if (state == "DOWN") 0 else -1)
        }

        {
            /**
             * Using leg matching algo (heur-3) menggunakan dice sorensen metric.
             */

            val matchedLegs = legs.filter { leg =>
//                if (leg.barCount == uncompletedLeg.barCount)
//                    println("sim: " + leg.barPattern.mkString("") + " vs " + uncompletedLeg.barPattern.mkString("") + " " + (DiceSorensenMetric.compare(leg.barPattern.mkString(""), uncompletedLeg.barPattern.mkString("")))(1).get )
                (leg.fractalCount < (uncompletedLeg.fractalCount+3)) &&
                leg.fractalPattern.startsWith(uncompletedLeg.fractalPattern) &&
                    (leg.barCount > uncompletedLeg.barCount) &&
                    (DiceSorensenMetric.compare(leg.barPattern, uncompletedLeg.barPattern)(1).get > 0.7)
            }

            val matchedLegsStats = matchedLegs.groupBy { leg =>
                DiceSorensenMetric.compare(leg.barPattern, uncompletedLeg.barPattern)(1).get
            }.filter(_._2.length > 3)

            var fractalSum = 0
            var probUp = 0
            var probDown = 0

            probUp += matchedLegs.count { l =>
                l.barPattern(uncompletedLeg.barCount) == 0x01
            }
            probDown += matchedLegs.count { l =>
                l.barPattern(uncompletedLeg.barCount) == 0x00
            }

            matchedLegsStats.toSeq.sortBy(_._2.length)
                .reverse.foreach { case (hash, _legs) =>

//                println("%d. (%s) %s".format(_legs.length, hash, _legs(0)))

                fractalSum += _legs.map(_.fractalCount).sum


                _legs.groupBy(_.barPattern.mkString("")).toSeq.sortBy(_._2.length).reverse
                    .foreach { case (bpat, _legs2) =>

//                    println("    %s - %s".format(bpat, _legs2.length))

                }
            }

//            val x = matchedLegsStats.flatMap(_._2)
//            println("    pattern: " + matchedLegsStats.flatMap(_._2).map(_.position).mkString(" "))
            val (down, up) = matchedLegsStats.flatMap(_._2).map(_.position).partition(_ == "up")

            val downSize = down.size
            val upSize = up.size



//            val delta = ( (downSize + upSize) / 1.7 )
            val delta = 0.0

            val state = {

                if (upSize > (downSize + delta) ) "UP"
                else if ((upSize + delta) < downSize) "DOWN"
                else "-"

            }

            val deltaUp = upSize + delta
            val deltaDown = downSize + delta

            val stateInt = if (state == "UP") 1 else if (state == "DOWN") 0 else -1

            val pips =
                stateInt match {
                    case 1 =>
                        matchedLegsStats.toSeq.flatMap(_._2).filter(_.position=="up").head.pips
                    case 0 =>
                        matchedLegsStats.toSeq.flatMap(_._2).filter(_.position=="down").head.pips
                    case _ => 0.0
                }

//            val fractalSumRounded = fractalSum / ((downSize + upSize) - stateInt)
//            val fractalSumRounded2 = fractalSum / (((downSize + upSize) / 3) - stateInt)

            println("MANTICORE HEUR-3 (%d/%d) delta(%s/%s), fsum(%s), prob(%d/%d), ds(%d), pips(%f) --> %s".format(up.size, down.size,
                deltaUp, deltaDown, fractalSum, /*fractalSumRounded, fractalSumRounded2,*/ probUp, probDown,
                matchedLegs.length, pips, state))

            allAlgoResults.+=(stateInt)
        }

        val state = {
            val r = allAlgoResults.result()
//            println(r.toSeq)
            if (r.count(_ == 1) > r.count(_ == 0)){
                "UP"
            }else if (r.count(_ == 1) < r.count(_ == 0)){
                "DOWN"
            }else{
                "-"
            }
        }
        println("------------------------------")
        println("PROBABILITY: " + state)
        println("------------------------------")

    }



    def main(args: Array[String]) {

        val untilDate = {
            if (args.length > 1)
                args(1)
            else
                "-"
        }

        // --nopm for not using pattern match at all
        val usingPatternMatch = !args.contains("--nopm")

        val data = new CsvReader(args(0), untilDate).toArray.toIndexedSeq

        val start = System.currentTimeMillis()

        try {
            process(data, usingPatternMatch)
        }catch{
            case e:ManticoreException =>
                System.err.println(e.getMessage)
        }finally{
            actorSystem.shutdown()
            NonBlockingChromosomeFinder.system.shutdown()
        }

        println("")
        println("Done in " + (System.currentTimeMillis() - start) + "ms")
        println("\n")

    }
//
//    def getProbLeg(legs:Seq[Leg]) = {
//
//        var buff = new ArrayBuffer[Byte]
//        val numFracAvg = legs.map(_.fractalCount).sum / legs.length
//        val numBarAvg = legs.map(_.barCount).sum / legs.length
//
//        var i = 0
//        while(i < numBarAvg){
//            val bp = legs.map { x =>
//                if (i < x.barPattern.length) {
//                    x.barPattern(i)
//                } else {
//                    0xFF
//                }
//            }
//            if (bp.filter(_ == 0x01).length > bp.filter(_ == 0x00).length){
//                buff += 0x01
//            }else{
//                buff += 0x00
//            }
//            i = i + 1
//        }
//
//        Leg("xxx", numFracAvg, numBarAvg, Array.empty[Byte], buff.result().toArray)
//
//    }
//
    // Calculate a sum of set bits of XOR'ed bytes
    def hammingDistance(b1: Array[Byte], b2: Array[Byte]) = {
        (b1.zip(b2).map((x: (Byte, Byte)) => numberOfBitsSet((x._1 ^ x._2).toByte))).sum
    }

    // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
    def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum
}
