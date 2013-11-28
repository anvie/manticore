package com.ansvia.manticore

import scala.collection.mutable.ArrayBuffer
import akka.actor.{Props, ActorSystem, Actor}
import akka.routing.RoundRobinRouter
import com.ansvia.manticore.Manticore.DNA
import java.util.concurrent.CountDownLatch

/**
 * Author: robin
 * Date: 11/11/13
 * Time: 11:04 PM
 *
 */
object Jj2 {

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



    def process(data:IndexedSeq[Record], usingPatternMatch:Boolean, loop:Int=5){

        val bars = FractalFinder.find(data)

        var fractals = bars.filter(_.isInstanceOf[Fractal]).map(_.asInstanceOf[Fractal]).map(_.pos)
        var nextFractals = Seq.newBuilder[Int]

        var i = 0
        while (i < loop){

            println("loop #" + i)

            val fractalSize = fractals.size
            val fractalStrings = (for (i <- 4 to 13)
                yield (i, fractals.slice(fractalSize-i, fractals.size).mkString(""))).toMap

            bars.slice(bars.size-30, bars.size).foreach(println)
            println("---------------------------------------")
            fractalStrings.foreach { case (i, d) => println("%d-strings -> %s".format(i, d)) }

            val dnas = Manticore.getDnas(new InlineDataSource(fractals.toSeq), 7)

            val (a,b,c) = Manticore.breakDown(dnas.slice(0, 10),
                fractals)

            println("---------------------------------------")

            println("up: %d, down: %d, proceed: %d, probability: %s".format(a,b,c, if (a > b) "UP" else if (a==b) "-" else "DOWN"))

            val nextPos = if (a>b) 1 else if (a<b) 0 else -1

            fractals ++= Seq(nextPos)

            nextFractals += nextPos

            i = i + 1
        }

        println("next fractals probability: {" + nextFractals.result().mkString(",") + "}")
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

        println("Done in " + (System.currentTimeMillis() - start) + "ms")
        println("\n")

    }


    // Calculate a sum of set bits of XOR'ed bytes
    def hammingDistance(b1: Array[Byte], b2: Array[Byte]) = {
        (b1.zip(b2).map((x: (Byte, Byte)) => numberOfBitsSet((x._1 ^ x._2).toByte))).sum
    }

    // 1 iteration for each bit, 8 total. Shift right and AND 1 to get i-th bit
    def numberOfBitsSet(b: Byte) : Int = (0 to 7).map((i : Int) => (b >>> i) & 1).sum
}
