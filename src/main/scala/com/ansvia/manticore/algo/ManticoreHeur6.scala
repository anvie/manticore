package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.DataGenerator


/**
 * HEUR-6 improvements version of HEUR-3
 */
class ManticoreHeur6(dataGenSource:DataGenerator, dataGenTarget:DataGenerator, debugMode:Boolean=false)
    extends ManticoreAlgo(dataGenSource, dataGenTarget)
        with ZZLegOp with AI with FractalOp {

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

    //    def train(posTime:String, result:Result)
    def correctPrevious(result: Result){
        prevResult = result
    }

    case class State(fractal:Fractal)

    lazy val unknown = Result(Direction.NEUTRAL, 0.0)

//    private var timePunch = new ArrayBuffer[Int]()
//    private var stableLength:Int = 0
    private var prevState:State = _



    private lazy val historyDataCandleBit = dataGenSource.chunkedData.map(_.bit)
    private lazy val historyDataFractalBit = FractalFinder.find(dataGenSource.chunkedData, includeNonFractalBar = false)
        .filter(_.isInstanceOf[Fractal])
        .map(_.asInstanceOf[Fractal]).map(_.pos)


//    def getStableLength = {
////        timePunch.result().toSeq.groupBy(x => x)
////            .toSeq.sortBy(_._1).reverse.map(_._1)
////            .headOption.getOrElse(0)
//        val s = timePunch.result()
//        if (s.length > 0)
//            if (s.length > 1)
//                s(s.length-2)
//            else
//                s(s.length-1)
//        else
//            0
//    }

    def calculate(posTime:String) = {

        try {

            if (posTime == "07.11.2013 22:30:00.000"){
                println("break")
            }

            var rv = prevResult

            val ts = Util.parseTime(posTime).getTime

            implicit lazy val currentData = dataGenTarget.chunkedData.filter(_.timestamp <= ts)
            lazy val currentDataDeltaWithSource = dataGenTarget.chunkedData.filter(d => d.timestamp > dataGenTarget.endTs && d.timestamp <= ts)

            lazy val currentDataFractal = getCurrentDataFractal(ts)
            lazy val currentDataFractalBit = currentDataFractal.map(_.pos)
            lazy val currentDataCandleBit = currentData.map(_.bit)

            lazy val currentDataDeltaCandleBit = currentDataDeltaWithSource.map(_.bit)
            lazy val currentDataDeltaFractalBit = fractalData.filter(f => f.timestamp <= ts && f.timestamp > dataGenTarget.endTs).map(_.pos)

//            val lastLeg = getLastLeg(ts)
//
//            val uLeg = getUncompletedLeg(lastLeg.timestamp)
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
////
//            val mLegs = legs.filter { leg =>
//                leg.fractalPattern.mkString("").startsWith(combinedFractalPattern.mkString("")) &&
//                    leg.length == (lastLeg.length + uLeg.length)
////                    leg.direction == lastLeg.direction &&
////                    DiceSorensenMetric.compare(leg.barPattern, lastLeg.barPattern++uLeg.barPattern)(1).getOrElse(0.0) > 0.95
//            }
//
//
//
//            val (fixedLegs, nonFixedLegs) = mLegs.partition { leg =>
//                leg.fractalPattern.mkString("") == combinedFractalPattern.mkString("") &&
//                    leg.length == (lastLeg.length + uLeg.length)
//            }

            // all match count, fixed count, non fixed count
//            print("(%d,%d,%d)".format(mLegs.length,fixedLegs.length,nonFixedLegs.length))

//            print(" (%d)".format(fixedLegs.length))

//            d(" [%d|%d|%d]".format(mLegs.length, fixedLegs.length, nonFixedLegs.length))





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

//            val currentFractal = fractalData.find(f => f.timestamp == ts)
//            val prevFractal = if (prevState != null) prevState.fractal else null
//
//            if (currentFractal.isDefined && prevFractal != null && prevFractal.pos != currentFractal.get.pos){

                val dnas1 = for (i <- 4 to 25)
                    yield currentDataFractal.slice(currentDataFractal.size-i,
                        currentDataFractal.size).zipWithIndex.map { case (f, ii) =>
                        (f.pos, ((currentDataFractal.length-i) + ii).toLong )
                    }.toSeq


                d2.println("fractal dna leg %s: ----------------".format(posTime))

                dnas1.foreach(dna => d2.println(dna.map(_._1).mkString("")))

                val sourceAndTargetCandleBit = historyDataCandleBit ++ currentDataDeltaCandleBit
                val sourceAndTargetFractalBit = historyDataFractalBit ++ currentDataDeltaFractalBit

                val (up1,down1,_) = Manticore.breakDown(dnas1, sourceAndTargetCandleBit, silent=true)
                val (up2,down2,_) = Manticore.breakDown(dnas1, sourceAndTargetFractalBit, silent=true)

//                val dnas2 = (for (i <- 7 to 25)
//                        yield currentData.slice(currentData.size-i,
//                                currentData.size).zipWithIndex.map { case (d, ii) =>
//                                (d.bit, ((currentData.length-i) + ii).toLong )
//                            }).toSeq

//                val (up3,down3,_) = Manticore.breakDown(dnas2, historyDataCandleBit, silent=true)

                val up = up1 + down2 //+ down3 + down2
                val down = down1 + up2 //+ up3 + up2

                val dir =
                    if (up > down) Direction.UP
                    else if (up < down) Direction.DOWN
                    else Direction.NEUTRAL

                if (dir != Direction.NEUTRAL)
                    rv = Result(dir, 0.0)
//            }


//            prevState = State(currentFractal.getOrElse(prevFractal))

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


    def close(){
        Manticore.shutdown()
        d2.close()
    }
}


