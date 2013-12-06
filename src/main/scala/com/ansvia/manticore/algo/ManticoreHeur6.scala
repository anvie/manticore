package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Leg
import scala.collection.mutable.ArrayBuffer


/**
 * HEUR-6 improvements version of HEUR-3
 */
class ManticoreHeur6(dataGenSource:DataGenerator, dataGenTarget:DataGenerator, debugMode:Boolean=false)
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

    lazy val unknown = Result(Direction.NEUTRAL, 0.0)

    private var timePunch = new ArrayBuffer[Int]()
    private var stableLength:Int = 0
    
    def getStableLength = {
//        timePunch.result().toSeq.groupBy(x => x)
//            .toSeq.sortBy(_._1).reverse.map(_._1)
//            .headOption.getOrElse(0)
        val s = timePunch.result()
        if (s.length > 0)
            if (s.length > 1)
                s(s.length-2)
            else
                s(s.length-1)
        else
            0
    }

    def calculate(posTime:String) = {

        try {

            if (posTime == "07.11.2013 22:30:00.000"){
                println("break")
            }

            var rv = prevResult

            val ts = Util.parseTime(posTime).getTime

            implicit lazy val currentData = dataGenTarget.chunkedData.filter(_.timestamp <= ts)

            val lastLeg = getLastLeg(ts)

            val uLeg = getUncompletedLeg(lastLeg.timestamp)

            lazy val lastLegOppositeDirection =
                lastLeg.direction match {
                    case Direction.UP => Direction.DOWN
                    case Direction.DOWN => Direction.UP
                }


//            if (lastLegOppositeDirection == prevResult.direction){
//                // clear timePunch
//                timePunch.clear()
//            }

            val combinedFractalPattern = lastLeg.fractalPattern ++ uLeg.fractalPattern

            val mLegs = legs.filter {
                leg =>
                    leg.fractalPattern.mkString("").startsWith(combinedFractalPattern.mkString("")) &&
                        leg.length == (lastLeg.length + uLeg.length) &&
                        leg.direction == lastLeg.direction &&
                        DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern ++ uLeg.barPattern)(1).getOrElse(0.0) > 0.9
            }

            val fixedLegs = mLegs.filter { leg =>
                leg.fractalPattern.mkString("") == combinedFractalPattern.mkString("") &&
                leg.length == (lastLeg.length + uLeg.length)
            }

            d("[%d|%d]".format(mLegs.length, fixedLegs.length))

//            val gLegs = matchedLegs.groupBy(_.fractalPattern)

//            print(" (" + mLegs.length + ")")

            if (timePunch.length > 10){
                timePunch.clear()
            }
            
            timePunch.+=(mLegs.length)

            if (mLegs.length > 0 && mLegs.length < 40){

                rv = Result(lastLegOppositeDirection, 0.0)

            }

            prevResult = rv

            rv
        }catch{
            case e:Ignored =>
                prevResult
        }

    }


    def d(text:String){
        if (debugMode)
            print(text)
    }


}
