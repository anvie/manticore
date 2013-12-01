package com.ansvia.manticore.tester

import com.ansvia.manticore._
import java.text.SimpleDateFormat
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import com.ansvia.manticore.Record

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 5:41 PM
 *
 */
class AlgoTester(dataGen:DataGenerator, algo:ManticoreAlgo) {

    case class TesterResult(var passed:Int, var missed:Int)

    lazy val legIterator = dataGen.zzLegsChunked.toIterator


    def play():TesterResult = {

        var curPos = 0

        var passes = 0
        var misses = 0


        while(legIterator.hasNext){
            val leg = legIterator.next()

            for (i <- curPos to leg.barCount - 1){
                if (algo.calculate(i).direction == leg.direction)
                    passes = passes + 1
                else
                    misses = misses + 1
            }

            curPos = curPos + leg.barCount

        }

        TesterResult(passes, misses)
    }


}

object Direction {
    val UP = 1
    val DOWN = 0
    val NEUTRAL = -1
}



abstract class ManticoreAlgo {

    /**
     * Manticore algo result
     * @param direction swing direction @see [[com.ansvia.manticore.tester.Direction]]
     * @param pips pips information if any.
     */
    case class Result(direction:Int, pips:Double)

    def calculate(pos:Int):Result

}

class DataGenerator(data:IndexedSeq[Record], startTime:String="", endTime:String=""){
    private val formatter = new SimpleDateFormat("yyyy.MM.dd HH:mm")
    val startTs = formatter.parse(startTime).getTime
    val endTs = formatter.parse(endTime).getTime

    /**
     * contains data that has been sliced out
     * using startTime and endTime.
     */
    lazy val chunkedData = {
        if (startTime.length > 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp > startTs && d.timestamp < endTs
            }
        }else if (startTime.length > 0 && endTime.length == 0){
            data.filter { d =>
                d.timestamp > startTs
            }
        }else if (startTime.length == 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp < endTs
            }
        }else{
            data
        }
    }
    lazy val zzfRaw = new ZigZagFinder(data, 13, 8, 5)
    lazy val zzLegsRaw = zzfRaw.getLegs
    lazy val zzfChunked = new ZigZagFinder(chunkedData, 13, 8, 5)
    lazy val zzLegsChunked = zzfChunked.getLegs

    lazy val lastLegRaw = zzLegsRaw(zzLegsRaw.length - 1)
    lazy val lastLegChunked = zzLegsChunked(zzLegsChunked.length - 1)

    /**
     * Unidentified leg a.k.a uncompleted leg.
     */
    lazy val uLeg = {
        val trailingData = data.filter(_.timestamp > lastLegRaw.timestamp)

        var fractals = FractalFinder.find(trailingData)
        if (fractals(fractals.length-1).isInstanceOf[Fractal]){
            fractals = fractals.slice(0, fractals.length-2)
        }

        val fractalPattern = fractals.filter(_.isInstanceOf[Fractal])
            .map(_.asInstanceOf[Fractal]).map(_.pos)
        val barPattern = trailingData.map(_.bit)
        val fractalCount = fractalPattern.length
        val barCount = barPattern.length

        Leg("-", fractalCount, barCount, fractalPattern.map(_.toByte), barPattern.map(_.toByte).toArray, 0.0)
    }

}

/**
 * Using leg matching algo (heur-3) menggunakan dice sorensen metric.
 */
class ManticoreHeur3(dataGen:DataGenerator) extends ManticoreAlgo {

    lazy val legs = dataGen.zzLegsChunked
    lazy val uLeg = dataGen.uLeg

    def calculate(pos: Int) = {


        val matchedLegs = legs.filter { leg =>
        //                if (leg.barCount == uncompletedLeg.barCount)
        //                    println("sim: " + leg.barPattern.mkString("") + " vs " + uncompletedLeg.barPattern.mkString("") + " " + (DiceSorensenMetric.compare(leg.barPattern.mkString(""), uncompletedLeg.barPattern.mkString("")))(1).get )
            (leg.fractalCount < (uLeg.fractalCount+3)) &&
                leg.fractalPattern.startsWith(uLeg.fractalPattern) &&
                (leg.barCount > uLeg.barCount) &&
                (DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).get > 0.7)
        }

        val matchedLegsStats = matchedLegs.groupBy { leg =>
            DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).get
        }.filter(_._2.length > 3)

        var fractalSum = 0
        var probUp = 0
        var probDown = 0

        probUp += matchedLegs.count { l =>
            l.barPattern(uLeg.barCount) == 0x01
        }
        probDown += matchedLegs.count { l =>
            l.barPattern(uLeg.barCount) == 0x00
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

        val (down, up) = matchedLegsStats.flatMap(_._2).map(_.direction).partition(_ == "up")

        val downSize = down.size
        val upSize = up.size



//            val delta = ( (downSize + upSize) / 1.7 )
        val delta = 0.0

        val nextDirection = {

            if (upSize > (downSize + delta) ) Direction.UP
            else if ((upSize + delta) < downSize) Direction.DOWN
            else Direction.NEUTRAL

        }

//        val deltaUp = upSize + delta
//        val deltaDown = downSize + delta

        val pips =
            nextDirection match {
                case Direction.UP =>
                    matchedLegsStats.toSeq.flatMap(_._2).find(_.direction== "up").map(_.pips).getOrElse(0.0)
                case Direction.DOWN =>
                    matchedLegsStats.toSeq.flatMap(_._2).find(_.direction == "down").map(_.pips).getOrElse(0.0)
                case _ => 
                    0.0
            }

        //            val fractalSumRounded = fractalSum / ((downSize + upSize) - stateInt)
        //            val fractalSumRounded2 = fractalSum / (((downSize + upSize) / 3) - stateInt)
//
//        println("MANTICORE HEUR-3 (%d/%d) delta(%s/%s), fsum(%s), prob(%d/%d), ds(%d), pips(%f) --> %s".format(up.size, down.size,
//            deltaUp, deltaDown, fractalSum, /*fractalSumRounded, fractalSumRounded2,*/ probUp, probDown,
//            matchedLegs.length, pips, state))
        
        Result(nextDirection, pips)
    }
}

