package com.ansvia.manticore

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import akka.actor.{Props, ActorSystem, Actor}
import akka.routing.RoundRobinRouter
import com.ansvia.manticore.Manticore.{DNAS, DNA}
import java.util.concurrent.CountDownLatch

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

        legs //.filter(leg => leg.fractalCount > 3 && leg.fractalCount < 14)
            .foreach { d =>

//            println("%d. %s".format(i, d))

            for(n <- 4 to 13){
                val aoeu = Manticore.getDnas(new InlineDataSource(d.fractalPattern.map(_.toInt).toSeq), n)
//                    .map(dd => dd.map(_._1))
                if (set2a.contains(n)){
                    val aoeu2 = set2a.get(n).get ++ aoeu
                    set2a.update(n, aoeu2)
                }else{
                    set2a += n -> aoeu
                }
            }

        }

//        val set2b = set1.filter { x =>
//            val p = x.map(_._1)
//            set2a.get(p.length).exists { pp =>
//                pp.contains(p)
//            }
//        }

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
        val trailingBarPattern = trailingBars.map(_.bit.toByte).toArray

        val finalPattern = legUsed.fractalPattern
        back = back + 1
        val legCount = 1
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


        if (pattBase.length < 3){
            throw ManticoreException("insufficient pattern for search.")
        }

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
            if (patt == pattBase &&
                (leg.barCount == legUsed.barCount) &&
                (leg.fractalCount == legUsed.fractalCount) &&
                (leg.time != legUsed.time)
            /*((leg.barCount - legUsed.barCount) < 10) &&
            (leg.fractalCount < (legUsed.fractalCount + 5)) &&
            (leg.fractalCount > legUsed.fractalCount)*/ ){

                val pattStr = patt.mkString(",")
                if (patternCount < 20){
                    println("   + found: " + leg)
                    if (patternCount == 19)
                        println("   + and more...")
                }
                patternCount = patternCount + 1
                if (ii < legsCount-1){
                    println(Console.YELLOW + "   + next-leg: " + legs(ii+1) + Console.RESET)
                }
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

        if (stats.size == 0)
            throw ManticoreException("No pattern match")

        var upCount = 0
        var downCount = 0

        set2a.foreach { case (l, ps) =>
            upCount += ps.count(_.map(_._1) == pattUp)
            downCount += ps.count(_.map(_._1) == pattDown)
        }

//        val (a,b,c) = Manticore.breakDown(set2a.flatMap(_._2).toSeq, data1, pattBase)

//        upCount += set2b.count(_.map(_._1) == pattUp)
//        downCount += set2b.count(_.map(_._1) == pattDown)

        println("up: " + upCount + ", down: " + downCount)
//        println("up: " + a + ", down: " + b + ", chroms: " + c)

        println("Statistics: ===")
        var i = 0
        for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,5) ){
            val count = leg._1
            val nextLegs = leg._3
            println(" %d \t- %s".format(count, patt))

            if (nextLegs != null){
                println("   \t   ch-leg: " + leg._2)
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
                probLegs.sortBy(_._2)/*.slice(0,5)*/.foreach {
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
                println("   \t   -> next leg prob: {" + getProbLeg(nextLegs) + "}")

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
    }



    def main(args: Array[String]) {

        val data = new CsvReader(args(0), args(1)).toArray.toIndexedSeq

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
