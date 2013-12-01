package com.ansvia.manticore.tester

import com.ansvia.manticore._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Leg
import com.ansvia.manticore.Manticore.DNAS
import scala.collection.mutable
import com.ansvia.manticore.algo.ManticoreAlgo

/**
 * Using leg matching algo (heur-3) menggunakan dice sorensen metric.
 */
class Fractal1(dataGen:DataGenerator) extends ManticoreAlgo {

    val name = "FRAC1"

    lazy val legs = dataGen.zzLegsRaw

    private var prevResult = Result(Direction.NEUTRAL, 0.0)

    def lastResult = prevResult

    def getUleg(pos:Int) = {
        var trailingData = dataGen.data.filter(_.timestamp > dataGen.chunkedData(pos).timestamp)

        if (trailingData.length > 10){
            trailingData = trailingData.slice(0, 10)
        }

        var fractals = FractalFinder.find(trailingData)
        if (fractals.length > 0 && fractals(fractals.length-1).isInstanceOf[Fractal]){
            fractals = fractals.slice(0, fractals.length-2)
        }

        val fractalPattern = fractals.filter(_.isInstanceOf[Fractal])
            .map(_.asInstanceOf[Fractal]).map(_.pos)
        val barPattern = trailingData.map(_.bit)
        val fractalCount = fractalPattern.length
        val barCount = barPattern.length

        Leg("-", fractalCount, barCount, fractalPattern.map(_.toByte), barPattern.map(_.toByte).toArray, 0.0)
    }

    // set2a hanya berisi data pattern dna bar dari legs.
    lazy val set2a = {
        var rv = new mutable.HashMap[Int,DNAS]

        for(n <- 2 to 14){
            val aoeu = Manticore.getDnas(new InlineDataSource(legs.flatMap(_.fractalPattern.map(_.toInt))), n)
            if (rv.contains(n)){
                val aoeu2 = rv.get(n).get ++ aoeu
                rv.update(n, aoeu2)
            } else {
                rv += n -> aoeu
            }
        }
        rv
    }
    
    def calculate(pos: Int) = {

        val uncompletedLeg = getUleg(pos)

        var upCount = 0
        var downCount = 0

        val finalPattern = uncompletedLeg.fractalPattern

        // diambil dari fractal pattern.
//        val pattBase: Seq[Int] = finalPattern.map(_.toInt).toSeq
        val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
        val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)


        // diambil dari candle pattern
//        val pattBar = uncompletedLeg.barPattern.map(_.toInt)
//        var pattBarSeq = Seq.empty[Seq[Int]]
//        if (pattBar.length >= 4){
//            //            pattBar = pattBar.reverse.slice(0, 7).reverse
//            for (ii <- 4 to pattBar.length){
//                pattBarSeq :+= pattBar.reverse.slice(0, ii).reverse.toSeq
//            }
//        }
//        val pattBarUp = pattBar ++ Seq(1)
//        val pattBarDown = pattBar ++ Seq(0)

//        println("\n")
//        println("base bar pattern: {"+ pattBar.mkString("") + "}")
//        pattBarSeq.zipWithIndex.foreach { case (p, i) =>
//            println("   %d-strings: %s".format(i+4, p.mkString("")))
//        }
//        println("up bar pattern: {" + pattBarUp.mkString("") + "}")
//        println("down bar pattern: {" + pattBarDown.mkString("") + "}")
//        println("\n")

        set2a.foreach { case (l, ps) =>
            //            ps.foreach { x =>
            //                if (x.map(_._1).length == pattBarUp.length)
            //                    println(x.map(_._1).mkString("") + " == " + pattBarUp.mkString(""))
            //            }
            upCount += ps.count(_.map(_._1) == pattUp)
            downCount += ps.count(_.map(_._1) == pattDown)
        }

        val nextDirection = if (upCount > downCount) Direction.UP
            else if (upCount < downCount) Direction.DOWN
            else Direction.NEUTRAL

        val rv = Result(nextDirection, 0.0)
        prevResult = rv
        rv
    }
}
