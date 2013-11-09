package com.ansvia.manticore

import org.apache.commons.io.FileUtils
import java.io.File
import com.ansvia.commons.logging.Slf4jLogger
import scala.collection.mutable.ArrayBuffer
import java.util.NoSuchElementException
import scala.collection.immutable


object FractalPos {
    val TOP = 1
    val BOTTOM = 0
    val TOP_AND_BOTTOM = 2

    def toStr(code:Int) = {
        code match {
            case TOP => "TOP"
            case BOTTOM => "BOTTOM"
            case TOP_AND_BOTTOM => "TOP & BOTTOM"
        }
    }
}

abstract class Bar(idx:Int)

/**
 * Fractal representative.
 * @param idx index.
 * @param time time.
 * @param pos fractal position @see [[com.ansvia.manticore.FractalPos]]
 */
case class Fractal(idx:Int, time:String, pos:Int) extends Bar(idx){
    override def toString:String = "Fractal(%d, %s, %s)".format(idx, time, FractalPos.toStr(pos))
}
case class NonFractal(idx:Int, time:String) extends Bar(idx) {
    override def toString: String = "-" + time + "-"
}

object FractalFinder extends Slf4jLogger {


    def extract(data:IndexedSeq[Record],size:Int) = {

        // index mulai dari belakang mundur

        val timeline = new Array[Bar](size)
        var i = size - 1
        //var foundUp = false
        //var foundDown = false
        var current:Double = 0.0

        while( i >= 2 ){

            var foundUp = false

            current = data(i).high

//            def update(pos:Int){
//                timeline(i) = Fractal(i, data(i).time, pos)
//            }

            /************************************************
              * PROCESS FRACTAL UP
              ***********************************************/

            if (current > high(data, i + 1) && current > high(data, i + 2) &&
                current > high(data, i - 1) && current > high(data, i - 2)){
                foundUp = true
//                update(1)
            }

            // find for 6 bars fractals
            if (!foundUp && (size - i - 1) >= 3){
                if (current == high(data,i+1) && current>high(data, i+2) && current>high(data, i+3) &&
                    current>high(data,i-1) && current>high(data, i-2)){
                    foundUp = true
//                    update(1)
                }
            }

            // find for 7 bars fractals

            if (!foundUp && (size - i - 1) >= 4){
                if (current >= high(data, i+1) && current==high(data, i+2) && current > high(data, i+3) &&
                    current>high(data, i+4) && current>high(data, i-1) && current>high(data, i-2)){
                    foundUp = true
//                    update(1)
                }
            }

            // find for 8 bars fractals

            if (!foundUp && (size - i - 1) >= 5){
                if (current>=high(data,i+1) && current==high(data,i+2) && current==high(data,i+3) &&
                    current>high(data, i+4) && current>high(data,i+5) &&
                    current>high(data, i-1) && current>high(data, i-2)){
                    foundUp = true
//                    update(1)
                }
            }


            // find for 9 bars fractals

            if (!foundUp && (size - i - 1) >= 6){
                if (current>=high(data, i+1) && current==high(data,i+2) && current>=high(data,i+3) &&
                    current==high(data, i+4) && current>high(data,i+5) && current>high(data,i+6) &&
                    current>high(data, i-1) && current>high(data, i-2)){
                    foundUp = true
//                    update(1)
                }
            }

            /************************************************
              * PROCESS FRACTAL DOWN
              ***********************************************/

            var foundDown = false
            current = data(i).low

            if (current<low(data,i+1) && current<low(data,i+2) &&
                current<low(data,i-1) && current<low(data,i-2)){
                foundDown = true
//                update(0)
            }

            // find for 6 bars fractal
            if (!foundDown && (size - i - 1) >= 3){
                if (current==low(data,i+1) && current<low(data,i+2) && current<low(data,i+3) &&
                    current<low(data,i-1) && current<low(data,i-2)){
                    foundDown = true
//                    update(0)
                }
            }

            // find for 7 bars fractal
            if (!foundDown && (size - i - 1) >= 4){
                if (current<=low(data,i+1) && current==low(data,i+2) && current<low(data,i+3) &&
                    current<low(data,i+4) &&
                    current<low(data,i-1) && current<low(data,i-2)){
                    foundDown = true
//                    update(0)
                }
            }

            // find for 8 bars fractal
            if (!foundDown && (size - i - 1) >= 5){
                if (current<=low(data,i+1) && current==low(data,i+2) && current==low(data,i+3) &&
                    current<low(data,i+4) && current<low(data,i+5) &&
                    current<low(data,i-1) && current<low(data,i-2)){
                    foundDown = true
//                    update(0)
                }
            }

            // find for 9 bars fractal
            if (!foundDown && (size - i - 1) >= 6){
                if (current<=low(data,i+1) && current==low(data,i+2) && current<=low(data,i+3) &&
                    current==low(data,i+4) && current<low(data,i+5) && current<low(data,i+6) &&
                    current<low(data,i-1) && current<low(data,i-2)){
                    foundDown = true
//                    update(0)
                }
            }

            if (!foundUp && !foundDown){
                timeline(i) = NonFractal(i, data(i).time)
            }else if (foundUp && !foundDown){
                timeline(i) = Fractal(i, data(i).time, FractalPos.TOP)
            }else if (foundDown && !foundUp){
                timeline(i) = Fractal(i, data(i).time, FractalPos.BOTTOM)
            }else if (foundUp && foundDown){
                timeline(i) = Fractal(i, data(i).time, FractalPos.TOP_AND_BOTTOM)
            }

            i = i - 1
        }

        timeline //.foreach(println)

    }

    def usage(){
        println("Usage:\n")
        println("      ./ffind [CSV-FILE]")
        sys.exit(2)
    }


    def main(args:Array[String]){


        import DataModes._

        val fileDataPath = args(0)


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

        println(" + total %d record loaded.".format(size))
        println(" + processing...")

        val timeline = extract(data, size)

        timeline.foreach(println)

        buff.clear()


    }

    def high(indexedData:IndexedSeq[Record], idx:Int) = {
        try {
            indexedData(idx).high
        }
        catch {
            case e:IndexOutOfBoundsException => 0.0
        }
    }

    def low(indexedData:IndexedSeq[Record], idx:Int) = {
        try {
            indexedData(idx).low
        }
        catch {
            case e:IndexOutOfBoundsException => 0.0
        }
    }

}
