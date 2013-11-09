package com.ansvia.manticore

import scala.actors.threadpool.AtomicInteger
import com.ansvia.manticore.Manticore.DNA
import akka.actor.{Props, Actor}
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import java.util.concurrent.CountDownLatch

/**
 * Author: robin
 * Date: 10/30/13
 * Time: 10:24 PM
 *
 */
trait ChromosomeFinder {

    val dna:DNA
    val index:Int
    val data:IndexedSeq[Int]
    val positivePattern:Seq[Int]
    val negativePattern:Seq[Int]
    val positives:AtomicInteger
    val negatives:AtomicInteger
    val chromosomes:AtomicInteger

    def start()

    def join()

    def run(){
        calculate()
    }

    def calculate(){

        val size = data.size

        val sLen = dna.length

        var tail = dna(sLen-1)._2

        var count = 0

        while(tail > 1){

            tail = tail - 1
            count = count + 1


//             print("   %02d %02d %02d %02d".format(dna))
//            println(dna)

//            val chrom = Seq(d4, d3, d2, d1)
            val chrom = dna.map { case (x, i) =>
                val idx = size - (i.toInt - count)
                if (idx < size)
                    data(idx)
                else
                    -1
            }

            if (positivePattern == chrom){
                positives.getAndIncrement
                //                    println(" +")
            }else if (negativePattern == chrom){
                negatives.getAndIncrement
                //                    println(" -")
                //                }else{
                //                    println("")
            }

            chromosomes.incrementAndGet()
        }
    }
}




class BlockingChromosomeFinder(val dna:DNA, val index:Int, val data:IndexedSeq[Int],
                               val positivePattern:Seq[Int], val negativePattern:Seq[Int],
                               val positives:AtomicInteger, val negatives:AtomicInteger,
                               val chromosomes:AtomicInteger)
    extends ChromosomeFinder {


    def start(){
        run()
    }

    def join(){}
}

class NonBlockingChromosomeFinder(val dna:DNA, val index:Int, val data:IndexedSeq[Int],
                                  val positivePattern:Seq[Int], val negativePattern:Seq[Int],
                                  val positives:AtomicInteger, val negatives:AtomicInteger,
                                  val chromosomes:AtomicInteger)
    extends ChromosomeFinder {

    import NonBlockingChromosomeFinder._


    import akka.util.duration._

    val lock = new CountDownLatch(1)

    def pattern = dna.toSeq.toString

    def start(){
        implicit val timeout = Timeout(5.seconds)
        workers ! Calculate(this)
    }

    def join(){
        lock.await()
    }
}

object NonBlockingChromosomeFinder {
    val system = akka.actor.ActorSystem.create("chromosome-finder")

    case class Calculate(cf:NonBlockingChromosomeFinder)

    case class Worker() extends Actor {
        protected def receive = {
            case Calculate(cf) => {
                cf.calculate()
                cf.lock.countDown()
            }
        }
    }

    val workers = system.actorOf(Props[Worker]
      .withRouter(RoundRobinRouter(nrOfInstances=4)))

}