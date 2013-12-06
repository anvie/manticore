package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.rockymadden.stringmetric.similarity.DiceSorensenMetric
import scala.collection.mutable.ArrayBuffer
import java.io._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.DataGenerator


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

    case class State(mLegsLength:Int, fixedLegsLength:Int, nonFixedLegsLength:Int)

    lazy val unknown = Result(Direction.NEUTRAL, 0.0)

    private var timePunch = new ArrayBuffer[Int]()
    private var stableLength:Int = 0
    private var prevState:State = _

    private lazy val fractalData = FractalFinder.find(dataGenTarget.chunkedData)
        .filter(_.isInstanceOf[Fractal])
        .map(_.asInstanceOf[Fractal])
    
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
            val fractalDataCurrent = fractalData.filter(_.timestamp <= ts)

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

            val (fixedLegs, nonFixedLegs) = mLegs.partition { leg =>
                leg.fractalPattern.mkString("") == combinedFractalPattern.mkString("") &&
                    leg.length == (lastLeg.length + uLeg.length)
            }

            d("[%d|%d|%d]".format(mLegs.length, fixedLegs.length, nonFixedLegs.length))

//            val gLegs = matchedLegs.groupBy(_.fractalPattern)

//            print(" (" + mLegs.length + ")")

//            if (timePunch.length > 10){
//                timePunch.clear()
//            }

            val ltp = if (prevState != null)
                prevState.mLegsLength
            else
                0

//            if ((mLegs.length > 0 &&
//                (ltp != mLegs.length)) || timePunch.length == 0){

                val dnas = for (i <- 4 to 13)
                    yield fractalDataCurrent.slice(fractalDataCurrent.size-i, fractalDataCurrent.size).map( f => (f.pos, f.idx) ).toSeq

                d2.println("dna: ----------------")
                dnas.foreach(dna => d2.println(dna.map(_._1).mkString("")))

                val (up,down,all) = Manticore.breakDown(dnas, currentData.map(_.bit), silent=true)

                val dir =
                    if (up > down) Direction.UP
                    else if (up < down) Direction.DOWN
                    else Direction.NEUTRAL

                if (dir != Direction.NEUTRAL)
                    rv = Result(dir, 0.0)

//            }

//            timePunch.+=(mLegs.length)
            prevState = State(mLegs.length, fixedLegs.length, nonFixedLegs.length)

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


    lazy val d2 = new FileLoggerOutput("/tmp/mth6-out-2.log")


    def close(){
        Manticore.shutdown()
        d2.close()
    }
}

class FileLoggerOutput(path:String) {
    val f = new File(path)

    if (f.exists())
        f.delete()

    val fw = new FileOutputStream(f)
    val bw = new PrintWriter(fw)

    def println(text:String) = {
        bw.println(text)
        bw.flush()
    }

    def print(text:String) = {
        bw.print(text)
        bw.flush()
    }

    def close(){
        fw.close()
        bw.close()
    }

}
