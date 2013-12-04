package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Leg


/**
 * HEUR-6 improvements version of HEUR-3
 */
class ManticoreHeur6(dataGenSource:DataGenerator, dataGenTarget:DataGenerator)
    extends ManticoreAlgo(dataGenSource, dataGenTarget)
        with ZZLegOp {

    val name = "MTH6"

    // only in range max startTs
    lazy val legs = {
        val rv = dataGenSource.zzLegsChunked
        rv
    }
//    val absOffset = (dataGen.data.size - dataGen.chunkedData.size) - 1
    var prevIsWrong = false

    private var prevResult:Result = Result(Direction.NEUTRAL, 0.0)

    def lastResult = prevResult


//    lazy val ensureProceed = dataGen.zzfRaw.process()

//    lazy val fractalsData = FractalFinder.find(dataGenTarget.data)
//        .filter(_.isInstanceOf[Fractal])
//        .map(_.asInstanceOf[Fractal])


    def calculate(posTime:String) = {

        try {

            var rv = prevResult

            val ts = Util.parseTime(posTime).getTime

            implicit val currentData = dataGenTarget.data.filter(_.timestamp <= ts)

            val lastLeg = getLastLeg(ts)

            val uLeg = getUncompletedLeg(ts)


            var matchedLegs = legs.filter { leg =>
                leg.fractalPattern.startsWith(lastLeg.fractalPattern ++ uLeg.fractalPattern) &&
                    leg.length == (lastLeg.length + uLeg.length) &&
//                    leg.direction == lastLeg.direction &&
                    DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern ++ uLeg.barPattern)(1).getOrElse(0.0) > 0.8
            }

            val (up, down) = matchedLegs.partition(_.direction == Direction.UP)

            val upSize = up.size
            val downSize = down.size

            val direction = if (upSize < downSize) Direction.UP
                else if (downSize < upSize) Direction.DOWN
                else Direction.NEUTRAL

            rv = Result(direction, 0.0)

            prevResult = rv

            rv
        }catch{
            case e:Ignored =>
                prevResult
        }

    }


}
