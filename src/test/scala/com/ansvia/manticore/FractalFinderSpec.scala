package com.ansvia.manticore

import org.specs2.mutable.Specification
import scala.collection.mutable.ArrayBuffer
import java.util.NoSuchElementException
import scala.collection.immutable
import org.specs2.specification.Scope

/**
 * Author: robin
 * Date: 11/9/13
 * Time: 11:28 AM
 *
 */
class FractalFinderSpec extends Specification {


    class Ctx extends Scope {

        val fileDataPath = "data/EURUSD1.csv"
        val csv = new CsvReader(fileDataPath)

        println(" + data source: " + fileDataPath)

        println(" + loading data into memory...")

        var buff = new ArrayBuffer[Record]()

        var rec:Record = null
        try {
            while (true) {
                rec = csv.nextRecord()
                //                println("  high: " + rec.high + ", low: " + rec.low)
                buff :+= rec
            }
        }
        catch {
            case e:NoSuchElementException =>
        }

        val data:immutable.IndexedSeq[Record] = buff.result().toIndexedSeq
        //        val data = data.reverse
        //        val data = data
        val size:Int = data.size
    }

    "Fractal finder" should {
        "extract fractals from tr" in new Ctx {
            val fractals = FractalFinder.extract(data, size)
                .filter(_.isInstanceOf[Fractal])
                .map(_.asInstanceOf[Fractal])
            val fractalSize = fractals.size
//            fractals.foreach(println)
            fractals(fractalSize - 1).pos must_== FractalPos.TOP
            fractals(fractalSize - 2).pos must_== FractalPos.BOTTOM
            fractals(fractalSize - 3).pos must_== FractalPos.TOP_AND_BOTTOM
            fractals(fractalSize - 4).pos must_== FractalPos.TOP
            fractals(fractalSize - 5).pos must_== FractalPos.BOTTOM
        }
    }

}
