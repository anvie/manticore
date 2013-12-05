package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Record
import com.ansvia.commons.logging.Slf4jLogger

/**
 * Author: robin
 * Date: 12/2/13
 * Time: 12:04 AM
 *
 */

/**
 * Manticore algo result
 * @param direction swing direction @see [[com.ansvia.manticore.Direction]]
 * @param pips pips information if any.
 */
case class Result(direction:Int, pips:Double)


abstract class ManticoreAlgo(dataGenSource:DataGenerator, dataGenTarget:DataGenerator) extends Slf4jLogger {


    protected var _dataGenTarget = dataGenTarget

    def calculate(posTime:String):Result

    def lastResult:Result

    def setDataGenTarget(dg:DataGenerator){
        _dataGenTarget = dataGenTarget
    }

}

trait ZZLegOp {

//    protected var _dataGenTarget:DataGenerator

    protected def getLastLeg(ts:Long)(implicit currentData:IndexedSeq[Record]) = {

        val zf = new ZigZagFinder(currentData).process()
        val legs = zf.getLegs

        if (legs.length == 0)
            throw new ManticoreException("cannot get last leg, legs length == 0")

        legs(legs.length - 1)

    }


    protected def getUncompletedLeg(ts:Long)(implicit currentData:IndexedSeq[Record]) = {
        var trailingData = currentData.filter(_.timestamp > ts)

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
}


trait AI {
    def train(posTime:String, result:Result)
    def guess(posTime:String):Option[Result]
    def markWrong()
}

