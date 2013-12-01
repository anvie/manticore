package com.ansvia.manticore.tester

import com.ansvia.manticore.{Direction, Leg, Fractal, FractalFinder}
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric

/**
 * Using leg matching algo (heur-3) menggunakan dice sorensen metric.
 */
class ManticoreHeur3(dataGen:DataGenerator) extends ManticoreAlgo {

    val name = "MTH3"

    lazy val legs = dataGen.zzLegsRaw

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

    def calculate(pos: Int) = {

        val uLeg = getUleg(pos)

        lazy val matchedLegs = legs.filter { leg =>
        //                if (leg.barCount == uncompletedLeg.barCount)
        //                    println("sim: " + leg.barPattern.mkString("") + " vs " + uncompletedLeg.barPattern.mkString("") + " " + (DiceSorensenMetric.compare(leg.barPattern.mkString(""), uncompletedLeg.barPattern.mkString("")))(1).get )
            (leg.fractalCount < (uLeg.fractalCount+3)) &&
                leg.fractalPattern.startsWith(uLeg.fractalPattern) &&
                (leg.barCount > uLeg.barCount) &&
                (DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).getOrElse(0.0) > 0.7)
        }


        val matchedLegsStats = matchedLegs.groupBy { leg =>
            DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).getOrElse(0.0)
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

//        matchedLegsStats.toSeq.sortBy(_._2.length)
//            .reverse.foreach { case (hash, _legs) =>
//
////                println("%d. (%s) %s".format(_legs.length, hash, _legs(0)))
//
//            fractalSum += _legs.map(_.fractalCount).sum
//
//
//            _legs.groupBy(_.barPattern.mkString("")).toSeq.sortBy(_._2.length).reverse
//                .foreach { case (bpat, _legs2) =>
//
////                    println("    %s - %s".format(bpat, _legs2.length))
//
//            }
//        }

        val (down, up) = matchedLegsStats.flatMap(_._2).map(_.direction).partition(_ == Direction.UP)

        val downSize = probUp // down.size
        val upSize = probDown // up.size



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
                    matchedLegsStats.toSeq.flatMap(_._2).find(_.direction== Direction.UP).map(_.pips).getOrElse(0.0)
                case Direction.DOWN =>
                    matchedLegsStats.toSeq.flatMap(_._2).find(_.direction == Direction.DOWN).map(_.pips).getOrElse(0.0)
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
