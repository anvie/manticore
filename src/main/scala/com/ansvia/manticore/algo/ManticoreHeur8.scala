package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.DataGenerator
import scala.collection.mutable.Queue

/**
 * MTH8 algorithm improvements of MTH7
 *
 * Using 3-repaint pattern detection for recalculation.
 *
 */
class ManticoreHeur8(dataGenSource:DataGenerator, dataGenTarget:DataGenerator, debugMode:Boolean=false)
    extends ManticoreAlgo(dataGenSource, dataGenTarget)
        with ZZLegOp with AI with FractalOp {

    val name = "MTH8"

    // only in range max startTs
    lazy val legs = {
        val rv = dataGenSource.zzLegsChunked
        rv
    }

//    val absOffset = (dataGen.data.size - dataGen.chunkedData.size) - 1
    var prevIsWrong = false

    private var prevResult:Result = Result(Direction.NEUTRAL, 0.0)

    def lastResult = prevResult


//    override var currentCandlePattern = ""
//    override var currentFractalPattern = ""
    var needToRecalculate = false
    private var recalculateCountDown = 0

    //    def train(posTime:String, result:Result)
    def correctPrevious(result: Result){
//        prevResult = result
        needToRecalculate = true
    }

    case class State(fractal:Fractal, lastLeg:Leg)

    lazy val unknown = Result(Direction.NEUTRAL, 0.0)

//    private var timePunch = new ArrayBuffer[Int]()
//    private var stableLength:Int = 0
    private var prevState:State = _

    private var bitStack = new Queue[Int]()


    private lazy val historyDataCandleBit = dataGenSource.chunkedData.map(_.bit)
    private lazy val historyDataFractalBit = FractalFinder.find(dataGenSource.chunkedData, includeNonFractalBar = false)
        .filter(_.isInstanceOf[Fractal])
        .map(_.asInstanceOf[Fractal]).map(_.pos)



    override def preTrain(){
        println("AI: Pre train setup...")
        _aiData.clear()
        _aiData ++= dataGenSource.zzLegsChunked.flatMap { leg =>
            val v = leg.barPattern.mkString("").grouped(4).toList.groupBy(x => x)
                .map(p => (p._1, p._2.length)).map { z =>

                (z._1, z._2.toDouble, Result(leg.direction, 0.0))
            }
            v
        }
        reloadAI()
        println("AI: training done.")
    }

    def getOppositeDirection(direction:Int) = {
        direction match {
            case Direction.UP => Direction.DOWN
            case Direction.DOWN => Direction.UP
            case _ => Direction.NEUTRAL
        }
    }

    var repaintCount = 0
    var afterCorrection = 0

    def calculate(posTime:String) = {

        try {

//            if (posTime == "07.11.2013 22:30:00.000"){
//                println("break")
//            }

            var rv = prevResult

            val ts = Util.parseTime(posTime).getTime

            implicit lazy val currentData = dataGenTarget.chunkedData.filter(_.timestamp <= ts)
            lazy val currentDataDeltaWithSource = dataGenTarget.chunkedData.filter(d => d.timestamp > dataGenTarget.endTs && d.timestamp <= ts)

            lazy val currentDataFractal = getCurrentDataFractal(ts)
            lazy val currentDataFractalBit = currentDataFractal.map(_.pos)
            lazy val currentDataCandleBit = currentData.map(_.bit)

            lazy val currentDataDeltaCandleBit = currentDataDeltaWithSource.map(_.bit)
            lazy val currentDataDeltaFractalBit = fractalData.filter(f => f.timestamp <= ts && f.timestamp > dataGenTarget.endTs).map(_.pos)

            val lastLeg = getLastLeg(ts)
//
            val uLeg = getUncompletedLeg(lastLeg.timestamp)
//
//            lazy val lastLegOppositeDirection =
//                lastLeg.direction match {
//                    case Direction.UP => Direction.DOWN
//                    case Direction.DOWN => Direction.UP
//                }


//            if (lastLegOppositeDirection == prevResult.direction){
//                // clear timePunch
//                timePunch.clear()
//            }
//
//            val combinedFractalPattern = lastLeg.fractalPattern ++ uLeg.fractalPattern
            val combinedBarPattern = lastLeg.barPattern ++ uLeg.barPattern
////
            lazy val mLegs = legs.filter { leg =>
                leg.fractalPattern.mkString("") == (uLeg.fractalPattern ++ Array(1)).mkString("") &&
//                    (leg.length < uLeg.length+3 && leg.length > uLeg.length-3 )
                    leg.length == uLeg.length
//                    leg.direction == lastLeg.direction &&
//                    DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern++uLeg.barPattern)(1).getOrElse(0.0) > 0.95
            }
//            lazy val mLegs2 = legs.filter { leg =>
//                leg.fractalPattern.mkString("") == (uLeg.fractalPattern ++ Array(0)).mkString("") &&
//                    (leg.length < uLeg.length+3 && leg.length > uLeg.length-3 )
////                    leg.direction == lastLeg.direction &&
////                    DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern++uLeg.barPattern)(1).getOrElse(0.0) > 0.95
//            }

//
//
//            val (fixedLegs, nonFixedLegs) = mLegs.partition { leg =>
//                leg.fractalPattern.mkString("") == combinedFractalPattern.mkString("") &&
//                    leg.length == (lastLeg.length + uLeg.length)
//            }

            // all match count, fixed count, non fixed count
//            print("(%d,%d,%d)".format(mLegs.length,fixedLegs.length,nonFixedLegs.length))

//            print(" (%s,%s)".format(currentDataFractalBit(currentDataFractalBit.length-1),currentDataCandleBit(currentDataCandleBit.length-1)))

//            d(" [%d|%d|%d]".format(mLegs.length, fixedLegs.length, nonFixedLegs.length))

            var timeToCalculate = false

            if (prevState != null){
                if (prevState.lastLeg.time != lastLeg.time){


                    timeToCalculate = true

                    if (prevState.lastLeg.direction != lastLeg.direction){
                        repaintCount = 0
                    }

                    recalculateCountDown = 2
                    repaintCount += 1

                    if (afterCorrection > 0)
                        afterCorrection -= 1

                    print(",")
                }
            }


//
//            if (fixedLegs.length > 5){
//
//                rv = Result(lastLegOppositeDirection, 0.0)
//
//            }else if (mLegs.length > 0){

//
//                val nextFixedLegs = fixedLegs.flatMap(_.nextLeg)
//                if (nextFixedLegs.map(_.length).contains(uLeg.length)){
//                    rv = Result(lastLeg.direction, 0.0)
//                }

//
//                mLegs =
//                    legs.filter { leg =>
//                        leg.fractalPattern.mkString("").startsWith(combinedFractalPattern.mkString("")) &&
////                            leg.length == (lastLeg.length + uLeg.length) &&
//                            DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern ++ uLeg.barPattern)(1).getOrElse(0.0) > 0.9
//                    }
//
//                val (ups, downs) = mLegs.partition(_.direction == Direction.UP)
//                val up = downs.length
//                val down = ups.length
//
//                val direction =
//                    if (up > down) Direction.UP
//                    else if (up < down) Direction.DOWN
//                    else prevResult.direction
//
//                rv = Result(direction, 0.0)


//            }


//            val gLegs = matchedLegs.groupBy(_.fractalPattern)

//            print(" (" + mLegs.length + ")")

//            if (timePunch.length > 10){
//                timePunch.clear()
//            }

            lazy val currentFractal = fractalData.find(f => f.timestamp == ts)
            lazy val prevFractal = if (prevState != null) prevState.fractal else null

            lazy val combinedPatGrouped = currentDataCandleBit.slice(currentDataCandleBit.length-100,currentDataCandleBit.length-1).mkString("").grouped(4).mkString(" ")
            val directionFromAI = predict(combinedPatGrouped)

            d2.println("direction from ai: " + Direction.toStr(directionFromAI))
            d2.println("ai pattern: " + aiPattern(combinedPatGrouped).mkString(""))
//
//            if (timeToCalculate){

//                recalculateCountDown = recalculateCountDown - 1

                val sourceAndTargetCandleBit = historyDataCandleBit ++ currentDataDeltaCandleBit
                val sourceAndTargetFractalBit = historyDataFractalBit ++ currentDataDeltaFractalBit

//                val dnas1 = (for(i <- 4 to 13)
//                    yield Manticore.getDnas(new InlineDataSource(sourceAndTargetFractalBit), i))
//                        .flatMap(x => x.slice(0, 50))


//                val combinedFractal = currentDataFractal.map(_.pos) ++ uLeg.fractalPattern.map(_.toInt)

                val dnas1 = for (i <- 4 to 13; if i % 2 == 0)
                    yield currentDataFractal.slice(currentDataFractal.size-i,
                        currentDataFractal.size).zipWithIndex.map { case (f, ii) =>
                        (f.pos, ((currentDataFractal.length-i) + ii).toLong )
                    }.toSeq

                d2.println("fractal dna leg %s: ----------------".format(posTime))

                dnas1.foreach(dna => d2.println(dna.map(_._1).mkString("")))

                val (up1,down1,_) = Manticore.breakDown(dnas1, sourceAndTargetCandleBit, silent=true)
                val (up2,down2,_) = Manticore.breakDown(dnas1, sourceAndTargetFractalBit, silent=true)
//
//                d2.println(" [%d,%d/%d,%d] (%d)".format(up1, down1, up2, down2, repaintCount))

//                val dnas2 = (for (i <- 4 to 25)
//                        yield currentData.slice(currentData.size-i,
//                                currentData.size).zipWithIndex.map { case (d, ii) =>
//                                (d.bit, ((currentData.length-i) + ii).toLong )
//                            }).toSeq
//
//                val (up3,down3,_) = Manticore.breakDown(dnas2, sourceAndTargetCandleBit, silent=true)


//
//                val upPref = if (uLeg.barPattern.length > 0) {
//                    uLeg.barPattern(0) match {
//                        case 0x00 => 5
//                        case 0x01 => 3
//                    }
//                }else{
//                    1
//                }
//                val downPref = if (uLeg.fractalPattern.length > 0) {
//                    uLeg.fractalPattern(0) match {
//                        case 0x00 => 11
//                        case 0x01 => 12
//                    }
//                }else{
//                    1
//                }

                val up = up1 - up2 //+ down3 + down2
                val down = down1 - down2 //+ up3 + up2

                val predictedDirection =
                    if (up > down) Direction.UP
                    else if (up < down) Direction.DOWN
                    else directionFromAI

                if (predictedDirection != Direction.NEUTRAL){
//                    if (swingCorner == 1){
//                        rv = Result(getOppositeDirection(dir), 0.0)
//                        swingCorner = swingCorner + 1
//                    }else
//                        rv = Result(predictedDirection, 0.0)
                }



                if (recalculateCountDown == 1 && !bitStack.isEmpty){

                    val ls = bitStack.toList.reverse

                    if (ls(0) == ls(1) && predictedDirection != ls(0)){
//                        val x = afterCorrection match {
//                            case z if z % 2 != 0 =>
//                                getOppositeDirection(predictedDirection)
//                            case 2 =>
//                                recalculateCountDown = 2
//                                predictedDirection
//                            case _ =>
//                                predictedDirection
//                        }
                        rv = Result(predictedDirection, 0.0)
//                        afterCorrection = 4
                    }
                }

                recalculateCountDown -= 1


                needToRecalculate = false

//            }

//            currentCandlePattern = currentDataCandleBit.slice(currentDataCandleBit.length-10,currentDataCandleBit.length).mkString("")
//            currentFractalPattern = currentDataFractalBit.slice(currentDataFractalBit.length-10,currentDataFractalBit.length).mkString("")



            prevState = State(currentFractal.getOrElse(prevFractal), lastLeg)

            bitStack.enqueue(rv.direction)

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


    lazy val d2 = new FileLoggerOutput("/tmp/mth6-out-dna.log")
    lazy val d3 = new FileLoggerOutput("/tmp/mth6-out-3.log")


    override def close(){
        Manticore.shutdown()
        d2.close()
    }
}


