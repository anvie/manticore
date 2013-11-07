package com.ansvia.manticore

import org.apache.commons.io.FileUtils
import java.io.File
import com.ansvia.commons.logging.Slf4jLogger
import scala.collection.mutable.ArrayBuffer
import java.util.NoSuchElementException


abstract class Bar(idx:Int)
case class Fractal(idx:Int, time:String, pos:Int) extends Bar(idx)
case class NonFractal(idx:Int, time:String) extends Bar(idx) {
    override def toString: String = "-" + time + "-"
}

object FractalFinder extends Slf4jLogger {



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
                println("  high: " + rec.high + ", low: " + rec.low)
                buff :+= rec
            }
        }
        catch {
            case e:NoSuchElementException =>
        }

        val data = buff.result().toIndexedSeq
//        val dataReversed = data.reverse
        val dataReversed = data
        val size = data.size

        println(" + total %d record loaded.".format(size))
        println(" + processing...")

        // index mulai dari belakang mundur


        val timeline = new Array[Bar](size)
        var i = size - 1
        var foundUp = false
        var foundDown = false
        var current:Double = 0.0

        while( i >= 2 ){

            foundUp = false

            current = data(i).high

            def update(pos:Int){
                timeline(i) = Fractal(i, data(i).time, pos)
            }

            /************************************************
             * PROCESS FRACTAL UP
             ***********************************************/

            if (current > high(dataReversed, i + 1) && current > high(dataReversed, i + 2) &&
                current > high(dataReversed, i - 1) && current > high(dataReversed, i - 2)){
                foundUp = true
                update(1)
            }

            // find for 6 bars fractals
            if (!foundUp && size - i - 1 >= 3){
//                if(dCurrent==High[i+1] && dCurrent>High[i+2] && dCurrent>High[i+3] &&
//                dCurrent>High[i-1] && dCurrent>High[i-2])
//                {
//                    bFound=true;
//                    ExtUpFractalsBuffer[i]=dCurrent;
//                }

                if (current == high(dataReversed,i+1) && current>high(dataReversed, i+2) && current>high(dataReversed, i+3) &&
                    current>high(dataReversed,i-1) && current>high(dataReversed, i-2)){
                    foundUp = true
                    update(1)
                }
            }

            // find for 7 bars fractals

            if (!foundUp && size - i - 1 >= 4){

//            if(!bFound && (Bars-i-1)>=4)
//            {
//                if(dCurrent>=High[i+1] && dCurrent==High[i+2] && dCurrent>High[i+3] && dCurrent>High[i+4] &&
//                dCurrent>High[i-1] && dCurrent>High[i-2])
//                {
//                    bFound=true;
//                    ExtUpFractalsBuffer[i]=dCurrent;
//                }
//            }
                if (current >= high(dataReversed, i+1) && current==high(dataReversed, i+2) && current > high(dataReversed, i+3) &&
                    current>high(dataReversed, i+4) && current>high(dataReversed, i-1) && current>high(dataReversed, i-2)){
                    foundUp = true
                    update(1)
                }
            }

            // find for 8 bars fractals

            if (!foundUp && size - i - 1 >= 5){
//                if(!bFound && (Bars-i-1)>=5)
//                {
//                    if(dCurrent>=High[i+1] && dCurrent==High[i+2] && dCurrent==High[i+3] && dCurrent>High[i+4] && dCurrent>High[i+5] &&
//                    dCurrent>High[i-1] && dCurrent>High[i-2])
//                    {
//                        bFound=true;
//                        ExtUpFractalsBuffer[i]=dCurrent;
//                    }
//                }

                if (current>=high(dataReversed,i+1) && current==high(dataReversed,i+2) && current==high(dataReversed,i+3) &&
                    current>high(dataReversed, i+4) && current>high(dataReversed,i+5) &&
                    current>high(dataReversed, i-1) && current>high(dataReversed, i-2)){
                    foundUp = true
                    update(1)
                }
            }


            // find for 9 bars fractals

            if (!foundUp && size - i - 1 >= 6){
//                if(!bFound && (Bars-i-1)>=6)
//                {
//                    if(dCurrent>=High[i+1] && dCurrent==High[i+2] && dCurrent==High[i+3] && dCurrent>High[i+4] && dCurrent>High[i+5] &&
//                    dCurrent>High[i-1] && dCurrent>High[i-2])
//                    {
//                        bFound=true;
//                        ExtUpFractalsBuffer[i]=dCurrent;
//                    }
//                }

                if (current>=high(dataReversed, i+1) && current==high(dataReversed,i+2) && current>=high(dataReversed,i+3) &&
                    current==high(dataReversed, i+4) && current>high(dataReversed,i+5) && current>high(dataReversed,i+6) &&
                    current>high(dataReversed, i-1) && current>high(dataReversed, i-2)){
                    foundUp = true
                    update(1)
                }
            }

            /************************************************
             * PROCESS FRACTAL DOWN
             ***********************************************/

            foundDown = false
            current = data(i).low

//            if(dCurrent<Low[i+1] && dCurrent<Low[i+2] && dCurrent<Low[i-1] && dCurrent<Low[i-2])
//            {
//                bFound=true;
//                ExtDownFractalsBuffer[i]=dCurrent;
//            }
//            //----6 bars Fractal
//            if(!bFound && (Bars-i-1)>=3)
//            {
//                if(dCurrent==Low[i+1] && dCurrent<Low[i+2] && dCurrent<Low[i+3] &&
//                dCurrent<Low[i-1] && dCurrent<Low[i-2])
//                {
//                    bFound=true;
//                    ExtDownFractalsBuffer[i]=dCurrent;
//                }
//            }

            if (current<low(dataReversed,i+1) && current<low(dataReversed,i+2) &&
                current<low(dataReversed,i-1) && current<low(dataReversed,i-2)){
                foundDown = true
                update(0)
            }

            // find for 6 bars fractal
            if (!foundDown && size - i - 1 >= 3){
                if (current==low(dataReversed,i+1) && current<low(dataReversed,i+2) && current<low(dataReversed,i+3) &&
                    current<low(dataReversed,i-1) && current<low(dataReversed,i-2)){
                    foundDown = true
                    update(0)
                }
            }

            // find for 7 bars fractal
            if (!foundDown && size - i - 1 >= 4){
                if (current<=low(dataReversed,i+1) && current==low(dataReversed,i+2) && current<low(dataReversed,i+3) &&
                    current<low(dataReversed,i+4) &&
                    current<low(dataReversed,i-1) && current<low(dataReversed,i-2)){
                    foundDown = true
                    update(0)
                }
            }

            // find for 8 bars fractal
            if (!foundDown && size - i - 1 >= 5){
                if (current<=low(dataReversed,i+1) && current==low(dataReversed,i+2) && current==low(dataReversed,i+3) &&
                    current<low(dataReversed,i+4) && current<low(dataReversed,i+5) &&
                    current<low(dataReversed,i-1) && current<low(dataReversed,i-2)){
                    foundDown = true
                    update(0)
                }
            }

            // find for 9 bars fractal
            if (!foundDown && size - i - 1 >= 6){
                if (current<=low(dataReversed,i+1) && current==low(dataReversed,i+2) && current<=low(dataReversed,i+3) &&
                    current==low(dataReversed,i+4) && current<low(dataReversed,i+5) && current<low(dataReversed,i+6) &&
                    current<low(dataReversed,i-1) && current<low(dataReversed,i-2)){
                    foundDown = true
                    update(0)
                }
            }
            
            if (!foundUp && !foundDown){
                timeline(i) = NonFractal(i, data(i).time)
            }

            i = i - 1
        }

        timeline.foreach(println)

        buff.clear()

    }

    def high(indexedData:IndexedSeq[Record], idx:Int) = {
        try {
            indexedData(idx-1).high
        }
        catch {
            case e:IndexOutOfBoundsException => 0.0
        }
    }

    def low(indexedData:IndexedSeq[Record], idx:Int) = {
        try {
            indexedData(idx-1).low
        }
        catch {
            case e:IndexOutOfBoundsException => 0.0
        }
    }

}
