package com.ansvia.manticore

import scala.actors.threadpool.AtomicInteger
import com.ansvia.manticore.Manticore.DNA

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

    protected def calculate(){

        val size = data.size

        var i1 = dna(3)._2.toInt
        var i2 = dna(2)._2.toInt
        var i3 = dna(1)._2.toInt
        var i4 = dna(0)._2.toInt

        if (i4 > 1){
            println("       (thread-%s) processing DNA #%d  %d(%02d) %d(%02d) %d(%02d) X(%02d)".format(
                Thread.currentThread().getId, index,
                dna(0)._1,dna(0)._2,dna(1)._1,dna(1)._2,dna(2)._1,dna(2)._2,dna(3)._2))
        }

        while(i4 > 1){

            i4 = i4 - 1
            i3 = i3 - 1
            i2 = i2 - 1
            i1 = i1 - 1

            val d4 = data(size - i4)
            val d3 = data(size - i3)
            val d2 = data(size - i2)
            val d1 = data(size - i1)

            //                print("   %02d %02d %02d %02d".format(d4, d3, d2, d1))

            val chrom = Seq(d4, d3, d2, d1)

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

        //            (positives.get(), negatives.get())
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

    private val _thread = new Thread(){
        override def run() {
            calculate()
        }
    }

    def start(){
        _thread.start()
    }

    def join(){
        _thread.join()
    }
}

