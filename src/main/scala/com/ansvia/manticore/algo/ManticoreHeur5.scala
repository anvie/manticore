package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Leg
import java.text.SimpleDateFormat


/**
 * HEUR-5 improvements version of HEUR-3
 */
class ManticoreHeur5(dataGenSource:DataGenerator, dataGenTarget:DataGenerator) extends ManticoreAlgo {

    val name = "MTH5"

    // only in range max startTs
    lazy val legs = {
//        println("before: " + dataGen.zzLegsRaw.length)
        val rv = dataGenSource.zzLegsChunked //.zzLegsRaw.filter(_.timestamp < dataGen.startTs)
//        println("after: " + rv.length)
//        println("last sim leg: " + rv(rv.length-1))
        rv
    }
//    val absOffset = (dataGen.data.size - dataGen.chunkedData.size) - 1
    var prevIsWrong = false

    private var prevResult:Result = Result(Direction.NEUTRAL, 0.0)

    def lastResult = prevResult

//    private var _aiRegs = Seq.empty[(String, Result)]
//
//    def train(pos:Int, result:Result){
//
//        val lastLeg = getLastLeg(pos).get
//
//        val signature = lastLeg.barPattern.mkString("")
//
//        _aiRegs :+= (signature, result)
//
//    }
//
//
//    def guess(pos: Int) = {
//        val lastLeg = getLastLeg(pos).get
//
//        val curSign = lastLeg.barPattern.mkString("")
//
//        val goodCandidates =
//            _aiRegs.filter { case (sign, _pos) =>
////                sign.length < (curSign.length + 5) &&
//                DiceSorensenMetric.compare(sign, curSign)(1).getOrElse(0.0) > 0.6
////                curSign.endsWith(uLeg.barPattern.mkString(""))
//            }
//
//        val up = goodCandidates.count(_._2.direction == Direction.UP)
//        val down = goodCandidates.length - up
//
//        val rv = if (up > down) Some(Result(Direction.UP, 0.0))
//        else if (up < down) Some(Result(Direction.DOWN, 0.0))
//        else None
//
////        rv.map { x =>
////            println("got from ai: " + x)
////        }
//
//        rv
//    }
//
//
//    def markWrong(){
//        prevIsWrong = true
//    }

    def getUleg(posTime:String) = {
        val ts = Util.parseTime(posTime).getTime
        var trailingData = dataGenTarget.data.filter(_.timestamp > ts)

        if (trailingData.length > 3){
            trailingData = trailingData.slice(0, 3)
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



    def getLastLeg(posTime:String) = {
        val ts = Util.parseTime(posTime).getTime
        dataGenTarget.zzLegsRaw.find(_.timestamp > ts)
    }

//    lazy val ensureProceed = dataGen.zzfRaw.process()

    lazy val fractalsData = FractalFinder.find(dataGenTarget.data)
        .filter(_.isInstanceOf[Fractal])
        .map(_.asInstanceOf[Fractal])

    def calculate(posTime:String) = {

        try {

            //        ensureProceed

            // check is fractal?
            // alo interest in fractal, non fractal point will be ignored

            //        val targetRecord = dataGen.data.find(_.time == dataGen.chunkedData(pos).time)

            //        val isFractal = dataGen.zzfRaw.isFractal(dataGen.zzfRaw.getZigZagBuffer(pos))
            val isFractal = fractalsData.exists(_.time == posTime)

            if (!isFractal)
                throw new Ignored

            // get last leg
            val lastLeg = getLastLeg(posTime).getOrElse {
                throw new Ignored
            }

            val uLeg = getUleg(posTime)

            // attempt #1
            // mixin last leg bar pattern and uleg bar pattern

            //        var lookForBarPattern = lastLeg.barPattern ++ uLeg.barPattern

            // searching for pattern in history

            var matchedLegs = legs.filter { leg =>
                leg.fractalPattern.startsWith(lastLeg.fractalPattern ++ uLeg.fractalPattern) &&
                    leg.length == (lastLeg.length + uLeg.length) &&
                    leg.direction == lastLeg.direction &&
                    DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern ++ uLeg.barPattern)(1).getOrElse(0.0) > 0.8
            }

            // if any then just use it as master
            if (matchedLegs.length == 1){

                val rv = Result(lastLeg.direction, 0.0)

                prevResult = rv

                rv

            }else if (matchedLegs.length > 1){

                val (up, down) = matchedLegs.map(_.direction).partition(_ == Direction.UP)

                val upSize = up.size
                val downSize = down.size

                val direction =
                    if (upSize > downSize) Direction.UP
                    else if (upSize < downSize) Direction.DOWN
                    else Direction.NEUTRAL

                val rv = Result(direction, 0.0)

                prevResult = rv

                rv
            }else{

                // attempt #2
                // only using uleg

                // searching for pattern in history

                matchedLegs = legs.filter { leg =>
                    leg.fractalPattern.startsWith(uLeg.fractalPattern) &&
                        leg.length == uLeg.length &&
                        //                    leg.direction == uLeg.direction &&
                        DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).getOrElse(0.0) > 0.8
                }

                if (matchedLegs.length == 1){
                    val rv = Result(matchedLegs(0).direction, 0.0)

                    prevResult = rv

                    rv
                }else if (matchedLegs.length > 1){

                    val (up, down) = matchedLegs.map(_.direction).partition(_ == Direction.UP)

                    val upSize = up.size
                    val downSize = down.size

                    val direction =
                        if (upSize > downSize) Direction.UP
                        else if (upSize < downSize) Direction.DOWN
                        else Direction.NEUTRAL

                    val rv = Result(direction, 0.0)

                    prevResult = rv

                    rv
                }else{

                    // attempt #3
                    // only using last leg

                    // searching for pattern in history
                    matchedLegs = legs.filter { leg =>
                        leg.fractalPattern.startsWith(lastLeg.fractalPattern) &&
                            leg.length == lastLeg.length &&
                            leg.direction == lastLeg.direction &&
                            leg.barPattern == lastLeg.barPattern
                    }

                    //                println("matchedLegs.length: " + matchedLegs.length)

                    if (matchedLegs.length > 5){
                        // give opposite direction from last leg

                        val opposite = if (lastLeg.direction == Direction.UP) Direction.DOWN else Direction.UP
                        val rv = Result(opposite, 0.0)
                        prevResult = rv
                        rv

                    }else{

                        // get from previous result

                        //                    guess(pos).getOrElse(prevResult)
                        prevResult

                    }
                }

            }
        }catch{
            case e:Ignored =>
                prevResult
        }

    }


}
