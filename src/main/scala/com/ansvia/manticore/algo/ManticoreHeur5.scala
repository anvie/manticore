package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Leg




/**
 * Using leg matching algo (heur-3) menggunakan dice sorensen metric.
 */
class ManticoreHeur5(dataGen:DataGenerator) extends ManticoreAlgo {

    val name = "MTH5"

    // only in range max startTs
    lazy val legs = {
//        println("before: " + dataGen.zzLegsRaw.length)
        val rv = dataGen.zzLegsRaw.filter(_.timestamp < dataGen.startTs)
//        println("after: " + rv.length)
//        println("last sim leg: " + rv(rv.length-1))
        rv
    }
    val absOffset = (dataGen.data.size - dataGen.chunkedData.size) - 1

    private var prevResult:Result = Result(Direction.NEUTRAL, 0.0)

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
    
    def getLastLeg(pos:Int) = {
        dataGen.zzLegsRaw.find(_.timestamp > dataGen.chunkedData(pos).timestamp)
    }

    lazy val ensureProceed = dataGen.zzfRaw.process()

    def calculate(pos: Int) = {

        ensureProceed

        // check is fractal?
        // alo interest in fractal, non fractal point will be ignored

        val isFractal = dataGen.zzfRaw.isFractal(dataGen.zzfRaw.getZigZagBuffer(absOffset + pos))

        if (!isFractal)
            throw new Ignored

        // get last leg
        val lastLeg = getLastLeg(pos).getOrElse {
            throw new Ignored
        }
        
        val uLeg = getUleg(pos)

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
                    prevResult
                }
            }

        }

//        println(uLeg.barPattern.mkString(""))

//        lazy val matchedLegs = legs.filter { leg =>
//            (leg.fractalCount < (uLeg.fractalCount+3)) &&
//                leg.fractalPattern.startsWith(uLeg.fractalPattern) &&
//                (leg.barCount > uLeg.barCount) &&
//                (DiceSorensenMetric.compare(leg.barPattern, uLeg.barPattern)(1).getOrElse(0.0) > 0.8)
//        }
//

    }
}
