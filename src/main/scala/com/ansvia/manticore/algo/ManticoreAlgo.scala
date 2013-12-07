package com.ansvia.manticore.algo

import com.ansvia.manticore._
import com.ansvia.manticore.Fractal
import com.ansvia.manticore.Record
import com.ansvia.commons.logging.Slf4jLogger
import breeze.data.Example
import breeze.linalg.Counter
import breeze.classify.NaiveBayes

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

    var currentCandlePattern:String

    var currentFractalPattern:String

    def lastResult:Result

    def setDataGenTarget(dg:DataGenerator){
        _dataGenTarget = dataGenTarget
    }

    def close()
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
        var trailingData = currentData.filter(_.timestamp > (ts + 60000) )

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

trait FractalOp {

    protected var _dataGenTarget:DataGenerator

    protected lazy val fractalData = FractalFinder.find(_dataGenTarget.chunkedData)
        .filter(_.isInstanceOf[Fractal])
        .map(_.asInstanceOf[Fractal])

    def isFractal(ts:Long) = {

        val rv = fractalData.find(f => f.timestamp == ts)

        rv.isDefined
    }


    def getCurrentDataFractal(ts:Long)(implicit currentData:IndexedSeq[Record]) = {
        FractalFinder.find(currentData, includeNonFractalBar = false)
            .filter(_.isInstanceOf[Fractal])
            .map(_.asInstanceOf[Fractal])
    }

}

trait AI {
//    def train(posTime:String, result:Result)
//    def guess(posTime:String):Option[Result]

    var _aiData = Seq.newBuilder[(String, Double, Result)]
    var ml:NaiveBayes[Int,String] = _

    def correctPrevious(result:Result)


    def train(pattern:String,result:Result) = {

        _aiData ++= pattern.split(" ").groupBy(x => x).map(p => (p._1, p._2.length.toDouble, result) )

        reloadAI()

        ml
    }

    def reloadAI(){
        val data = _aiData.result()
        val (upData, downData) = data.partition(_._3.direction == Direction.UP)
        val trainedData = Array(
            Example(Direction.UP, Counter( upData.map(x => (x._1, x._2)) ) ),
            Example(Direction.DOWN, Counter( downData.map(x => (x._1, x._2)) ))
        )

        val nb = new NaiveBayes.Trainer[Int,String]()
        ml = nb.train(trainedData)
    }

    def aiPattern(pattern:String) = {
        val normPat = pattern.split(" ").groupBy(x => x)
            .map(p => Counter((p._1, p._2.length.toDouble)))

        val result = normPat.map(x => ml.classify(x))
        result
    }


    def predict(pattern:String) = {
        if (ml != null){
            val result = aiPattern(pattern)

            val up = result.count(_ == Direction.UP)
            val down = result.count(_ == Direction.DOWN)
            
            if (up > down) Direction.UP
                else if (up < down) Direction.DOWN
                else Direction.NEUTRAL
        }else
            Direction.NEUTRAL
    }


    def preTrain(){
        // override this
    }


}

