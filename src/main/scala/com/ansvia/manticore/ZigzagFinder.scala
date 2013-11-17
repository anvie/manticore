package com.ansvia.manticore

import scala.collection.mutable.ArrayBuffer

/**
 * Author: robin
 * Date: 11/10/13
 * Time: 12:56 AM
 *
 */

case class Leg(time:String, fractalCount:Int, barCount:Int, fractalPattern:Array[Byte], barPattern:Array[Byte]){
    override def toString = "leg[%s] = f: %d, bar: %d, pos: %s, fpatt: {%s}, bpatt: {%s}".format(time,
        fractalCount, barCount, position, fractalPattern.map(_.toString).mkString(","), barPattern.mkString(","))
    def position = {
        if (fractalPattern.length > 1){
            if (fractalPattern(fractalPattern.length-1) == 0x01)
                "up"
            else
                "down"
        }else if (fractalPattern.length == 1)
            if (fractalPattern(0) == 0x01) "up" else "down"
        else
            "-"
    }

}
case class Point(value:Double, fractalPos:Int){
    override def toString = {
        if (fractalPos==FractalPos.NONE){
            "-"
        }else{
            "Point(" + value + "," + FractalPos.toStr(fractalPos) + ")"
        }
    }
}

class ZigzagFinder(data:IndexedSeq[Record], depth:Int=13, deviation:Int=8, backstep:Int=5) {

    val size = data.length
    private var lowMapBuffer = new Array[Double](size + 1)
    private var highMapBuffer = new Array[Double](size + 1)
    private var zigzagMapBuffer = new Array[Point](size + 1)

    private var calculated = false

    def instrPips = 0.0

    def process() = {

        var lastLow = 0.0
        var lastHigh = 0.0
        var curLow = 0.0
        var curHigh = 0.0
        var lookFor = 0
        var idx = 0

        var shift = 0


        var lastHighPos = 0
        var lastLowPos = 0

        println("data size: " + size)

        while(shift < size - 1){

            idx = idx + 1

            // cari lowest value
            var v = lowest(shift)

            if (v == lastLow)
                v = 0.0
            else {
                lastLow = v
                if ((data(shift).low - v) > (deviation.toDouble * instrPips)){
                    v = 0.0
                }else{
                    for (back <- 1 to backstep){
                        if (shift - back > 0){
                            val res = lowMapBuffer(shift-back)
                            if (res != 0.0 && res > v){
                                lowMapBuffer(shift-back)=0.0
                            }
                        }
                    }
                }
            }

            if (data(shift).low==v){
                lowMapBuffer(shift) = v
            }else{
                lowMapBuffer(shift) = 0.0
            }

            // cari highest value
            v = highest(shift)

            if (v == lastHigh)
                v = 0.0
            else {
                lastHigh = v
                if ((v - data(shift).high) > (deviation.toDouble * instrPips))
                    v = 0.0
                else {
                    for (back <- 1 to backstep){
//                        println("back: " + back)
                        if (shift - back > 0){
                            val res = highMapBuffer(shift-back)
                            if (res != 0.0 && res < v){
                                highMapBuffer(shift-back)=0.0
                            }
                        }
                    }
                }
            }

            if (data(shift).high==v){
                highMapBuffer(shift) = v
            }else{
                highMapBuffer(shift) = 0.0
            }

            shift = shift + 1

        }


        // final cutting
        if (lookFor == 0){
            lastLow = 0.0
            lastHigh = 0.0
        }else{
            lastLow = curLow
            lastHigh = curHigh
        }

        shift = 0 //size - 1
        idx = 0

        var current = 0.0
        //        var fractalCount = 0
        var barCount = 0
        //        val zzlegs = new mutable.HashMap[Int, Leg]

        while(shift < size - 1){
//            var res = 0.0
            idx = idx + 1

            barCount += 1

            lookFor match {
                case 0 => { // look for peak or lawn
                    if (lastLow == 0.0 && lastHigh == 0.0){
                        if (highMapBuffer(shift) != 0.0){
                            lastHigh = data(shift).high
                            lastHighPos = shift
                            lookFor = -1
                            zigzagMapBuffer(shift) = Point(lastHigh, FractalPos.TOP)
//                            res = 1
                        }
                        if (lowMapBuffer(shift) != 0.0){
                            lastLow = data(shift).low
                            lastLowPos = shift
                            lookFor = 1
                            zigzagMapBuffer(shift) = Point(lastLow, FractalPos.BOTTOM)
//                            res = 1
                        }
                    }
                }
                case 1 => { // look for peak
                    if (lowMapBuffer(shift) != 0.0 && lowMapBuffer(shift)<lastLow && highMapBuffer(shift)==0.0){
                        zigzagMapBuffer(lastLowPos) = Point(0.0, FractalPos.NONE)
                        lastLowPos = shift
                        lastLow = lowMapBuffer(shift)
                        zigzagMapBuffer(shift) = Point(lastLow, FractalPos.BOTTOM)
//                        res = 1
                    }
                    if (highMapBuffer(shift)!=0.0 && lowMapBuffer(shift)==0.0){
                        lastHigh = highMapBuffer(shift)
                        lastHighPos = shift
                        zigzagMapBuffer(shift) = Point(lastHigh, FractalPos.TOP)
                        lookFor = -1
//                        res = 1
                    }
                }
                case -1 => { // look for lawn
                    if (highMapBuffer(shift)!=0.0 && highMapBuffer(shift)>lastHigh && lowMapBuffer(shift)==0.0){
                        zigzagMapBuffer(lastHighPos) = Point(0.0, FractalPos.NONE)
                        lastHighPos = shift
                        lastHigh = highMapBuffer(shift)
                        zigzagMapBuffer(shift) = Point(lastHigh, FractalPos.TOP)
                    }
                    if (lowMapBuffer(shift)!=0.0 && highMapBuffer(shift)==0.0){
                        lastLow = lowMapBuffer(shift)
                        lastLowPos = shift
                        zigzagMapBuffer(shift) = Point(lastLow, FractalPos.BOTTOM)
                        lookFor = 1
                    }
                }
            }

            shift = shift + 1
        }



        for(i <- 0 to size - 1){

            var foundUp = false

            //                val fShift = size - shift - 1
//            val i = size - shift - 1

            current = data(i).high


            /************************************************
              * PROCESS FRACTAL UP
              ***********************************************/

            if (data(i).time == "2013.11.15 22:05"){
                println("break")
            }

            if (current > high(data, i - 1) && current > high(data, i - 2) &&
                current > high(data, i + 1) && current > high(data, i + 2)){
                foundUp = true
                //                update(1)
            }

            // find for 6 bars fractals
            if (!foundUp && (size - i - 1) >= 3){
                if (current == high(data,i-1) && current>high(data, i-2) && current>high(data, i-3) &&
                    current>high(data,i+1) && current>high(data, i+2)){
                    foundUp = true
                    //                    update(1)
                }
            }

            // find for 7 bars fractals

            if (!foundUp && (size - i - 1) >= 4){
                if (current >= high(data, i-1) && current==high(data, i-2) && current > high(data, i-3) &&
                    current>high(data, i-4) && current>high(data, i+1) && current>high(data, i+2)){
                    foundUp = true
                    //                    update(1)
                }
            }

            // find for 8 bars fractals

            if (!foundUp && (size - i - 1) >= 5){
                if (current>=high(data,i-1) && current==high(data,i-2) && current==high(data,i-3) &&
                    current>high(data, i-4) && current>high(data, i-5) &&
                    current>high(data, i+1) && current>high(data, i+2)){
                    foundUp = true
                    //                    update(1)
                }
            }


            // find for 9 bars fractals

            if (!foundUp && (size - i - 1) >= 6){
                if (current>=high(data, i-1) && current==high(data,i-2) && current>=high(data,i-3) &&
                    current==high(data, i-4) && current>high(data,i-5) && current>high(data,i-6) &&
                    current>high(data, i+1) && current>high(data, i+2)){
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
                if (current==low(data,i-1) && current<low(data,i-2) && current<low(data,i-3) &&
                    current<low(data,i+1) && current<low(data,i+2)){
                    foundDown = true
                    //                    update(0)
                }
            }

            // find for 7 bars fractal
            if (!foundDown && (size - i - 1) >= 4){
                if (current<=low(data,i-1) && current==low(data,i-2) && current<low(data,i-3) &&
                    current<low(data,i-4) &&
                    current<low(data,i+1) && current<low(data,i+2)){
                    foundDown = true
                    //                    update(0)
                }
            }

            // find for 8 bars fractal
            if (!foundDown && (size - i - 1) >= 5){
                if (current<=low(data,i-1) && current==low(data,i-2) && current==low(data,i-3) &&
                    current<low(data,i-4) && current<low(data,i-5) &&
                    current<low(data,i+1) && current<low(data,i+2)){
                    foundDown = true
                    //                    update(0)
                }
            }

            // find for 9 bars fractal
            if (!foundDown && (size - i - 1) >= 6){
                if (current<=low(data,i-1) && current==low(data,i-2) && current<=low(data,i-3) &&
                    current==low(data,i-4) && current<low(data,i-5) && current<low(data,i-6) &&
                    current<low(data,i+1) && current<low(data,i+2)){
                    foundDown = true
                    //                    update(0)
                }
            }

            if (zigzagMapBuffer(i) == null || zigzagMapBuffer(i).value == 0.0){
                if (foundUp){
                    zigzagMapBuffer(i) = Point(0.0, FractalPos.TOP)
                }
                if (foundDown){
                    zigzagMapBuffer(i) = Point(0.0, FractalPos.BOTTOM)
                }
                if (!foundUp && !foundDown){
                    zigzagMapBuffer(i) = Point(-1, FractalPos.NONE)
                }
            }

            //                if (!foundUp && !foundDown){
            //                    timeline(i) = NonFractal(i, data(i).time)
            //                }else if (foundUp && !foundDown){
            //                    timeline(i) = Fractal(i, data(i).time, FractalPos.TOP)
            //                }else if (foundDown && !foundUp){
            //                    timeline(i) = Fractal(i, data(i).time, FractalPos.BOTTOM)
            //                }else if (foundUp && foundDown){
            //                    timeline(i) = Fractal(i, data(i).time, FractalPos.TOP_AND_BOTTOM)
            //                }
        }

        calculated = true

        this

    }

    def highest(shift:Int) = {
        var v = data(shift).high
        var i = shift - 1
        if (i > 0){
            try {
                while (i > shift - depth) {
                    v = math.max(v, data(i).high)
                    i = i - 1
                }
            }
            catch {
                case e:ArrayIndexOutOfBoundsException =>
                case e:IndexOutOfBoundsException =>
                //                    v = 0.0
            }
        }
        v
    }

    def lowest(shift:Int) = {
        var v = data(shift).low
        var i = shift - 1
        if (i > 0){
            try {
                while (i > shift - depth) {
                    v = math.min(v, data(i).low)
                    i = i - 1
                }
            }
            catch {
                case e:ArrayIndexOutOfBoundsException =>
                case e:IndexOutOfBoundsException =>
                //                    v = 0.0
            }
            //            i = shift + 1
            //            try {
            //                while (i < shift + depth) {
            //                    v = math.min(v, data(i).low)
            //                    i = i + 1
            //                }
            //            }
            //            catch {
            //                case e:ArrayIndexOutOfBoundsException =>
            ////                    v = 0.0
            //            }
        }
        v
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

    def getZigZagBuffer = zigzagMapBuffer

    private def isZzPoint(zz:Point) = {
        zz != null && zz.value != 0.0 && zz.value != 1.0 && zz.value != -1.0
    }

    private def isFractal(p:Point) = {
        p != null &&
        p.fractalPos != FractalPos.NONE
    }


//    private def getBarDirection(idx:Int) = {
//
//        data(idx).direction
//    }



    def getLegs = {

        if (!calculated){
            process()
        }

        var rv = new ArrayBuffer[Leg]
        var fractalPattern = new ArrayBuffer[Byte]
        var barPattern = new ArrayBuffer[Byte]

        var begin = false
        var fractalCount = 0
        var barCount = 0
        var i = 0

//        println("data.size: " + data.size)

        for (zzPoint <- zigzagMapBuffer){
            if (!begin){
                if (isZzPoint(zzPoint)){
                    begin = true
                    fractalPattern.clear()
                    fractalCount = 1
                    barCount = 1
                    barPattern :+= data(i).bit.toByte
                }
            }else if (begin && isZzPoint(zzPoint)){
//                begin = false
                fractalPattern :+= zzPoint.fractalPos.toByte
                barPattern :+= data(i).bit.toByte
//                fractalCount += 1
//                barCount += 1
                if (data(i).time == "2013.11.15 19:50"){
                    println("break")
                }
                rv :+= Leg(data(i).time, fractalCount + 1, barCount + 1, fractalPattern.toArray, barPattern.toArray)
                fractalCount = 0
                barCount = 0
                fractalPattern.clear()
                barPattern.clear()
            }else if (begin){
                if (isFractal(zzPoint)){
                    fractalCount += 1
                    fractalPattern :+= zzPoint.fractalPos.toByte
                }
                if (i < data.size){
                    barPattern :+= data(i).bit.toByte
                }
                barCount += 1
            }
            i = i + 1
        }

        rv.toArray
    }
}

object ZigZagFinder {
    def main(args: Array[String]) {

        val filePath = args(0)

        val csvr = new CsvReader(filePath)
        val data = csvr.toArray.toIndexedSeq
        val zzf = new ZigzagFinder(data)

        readLine("find for what? 1=zz, 2=legs : ").trim match {
            case "1" =>
                zzf.process().getZigZagBuffer.zipWithIndex.foreach { case (d, i) =>
                    if (i < data.length - 1){
                        println("%d. %s - %s".format(i, data(i).time, d))
                    }
                }

            case "2" =>
                zzf.getLegs.foreach(println)

        }



    }
}