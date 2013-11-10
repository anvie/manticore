package com.ansvia.manticore

import scala.collection.mutable

/**
 * Author: robin
 * Date: 11/10/13
 * Time: 12:56 AM
 *
 */

case class Leg(fractalCount:Int, barCount:Int)

class ZigzagFinder(data:Array[Record], depth:Int=12, deviation:Int=5, backstep:Int=8) {

    val size = data.length
    private var lowMapBuffer = new Array[Double](size + 1)
    private var highMapBuffer = new Array[Double](size + 1)
    private var zigzagMapBuffer = new Array[Double](size + 1)

    def instrPips = 0

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

        while(shift < size - 1){

            idx = idx + 1

            // cari lowest value
            var v = lowest(shift)

            if (v == lastLow)
                v = 0.0
            else {
                lastLow = v
                if ((data(shift).low - v) > (deviation.toDouble * instrPips.toDouble)){
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

//                var foundRedundant = false
//                for (back <- 1 to backstep; if !foundRedundant){
//                    if (shift - back > 0){
//                        foundRedundant = data(shift).low >= data(shift - back).low
//                    }
//                }
//                if (!foundRedundant)
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
                if ((v - data(shift).high) > (deviation * instrPips))
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

//                var foundRedundant = false
//                for (back <- 1 to backstep; if !foundRedundant){
//                    if (shift - back > 0){
//                        foundRedundant = data(shift).high <= data(shift - back).high
//                    }
//                }
//                if (!foundRedundant)
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
        var fractalCount = 0
        var barCount = 0
        val zzlegs = new mutable.HashMap[Int, Leg]

        while(shift < size - 1){
            var res = 0.0
            idx = idx + 1

            barCount += 1

            lookFor match {
                case 0 => { // look for peak or lawn
                    if (lastLow == 0.0 && lastHigh == 0.0){
                        if (highMapBuffer(shift) != 0.0){
                            lastHigh = data(shift).high
                            lastHighPos = shift
                            lookFor = -1
                            zigzagMapBuffer(shift) = lastHigh
                            res = 1
                        }
                        if (lowMapBuffer(shift) != 0.0){
                            lastLow = data(shift).low
                            lastLowPos = shift
                            lookFor = 1
                            zigzagMapBuffer(shift) = lastLow
                            res = 1
                        }
                    }
                }
                case 1 => { // look for peak
                    if (lowMapBuffer(shift) != 0.0 && lowMapBuffer(shift)<lastLow && highMapBuffer(shift)==0.0){
                        zigzagMapBuffer(lastLowPos) = 0.0
                        lastLowPos = shift
                        lastLow = lowMapBuffer(shift)
                        zigzagMapBuffer(shift) = lastLow
                        res = 1
                    }
                    if (highMapBuffer(shift)!=0.0 && lowMapBuffer(shift)==0.0){
                        lastHigh = highMapBuffer(shift)
                        lastHighPos = shift
                        zigzagMapBuffer(shift) = lastHigh
                        lookFor = -1
                        res = 1
                    }
                }
                case -1 => { // look for lawn
                    if (highMapBuffer(shift)!=0.0 && highMapBuffer(shift)>lastHigh && lowMapBuffer(shift)==0.0){
                        zigzagMapBuffer(lastHighPos) = 0.0
                        lastHighPos = shift
                        lastHigh = highMapBuffer(shift)
                        zigzagMapBuffer(shift) = lastHigh
                    }
                    if (lowMapBuffer(shift)!=0.0 && highMapBuffer(shift)==0.0){
                        lastLow = lowMapBuffer(shift)
                        lastLowPos = shift
                        zigzagMapBuffer(shift) = lastLow
                        lookFor = 1
                    }
                }
            }


            {

                var foundUp = false

                val fShift = size - shift - 1

                current = data(fShift).high


                /************************************************
                  * PROCESS FRACTAL UP
                  ***********************************************/

                if (current > high(data, fShift + 1) && current > high(data, fShift + 2) &&
                    current > high(data, fShift - 1) && current > high(data, fShift - 2)){
                    foundUp = true
                    //                update(1)
                }

                // find for 6 bars fractals
                if (!foundUp && (size - fShift - 1) >= 3){
                    if (current == high(data,fShift+1) && current>high(data, fShift+2) && current>high(data, fShift+3) &&
                        current>high(data,fShift-1) && current>high(data, fShift-2)){
                        foundUp = true
                        //                    update(1)
                    }
                }

                // find for 7 bars fractals

                if (!foundUp && (size - fShift - 1) >= 4){
                    if (current >= high(data, fShift+1) && current==high(data, fShift+2) && current > high(data, fShift+3) &&
                        current>high(data, fShift+4) && current>high(data, fShift-1) && current>high(data, fShift-2)){
                        foundUp = true
                        //                    update(1)
                    }
                }

                // find for 8 bars fractals

                if (!foundUp && (size - fShift - 1) >= 5){
                    if (current>=high(data,fShift+1) && current==high(data,fShift+2) && current==high(data,fShift+3) &&
                        current>high(data, fShift+4) && current>high(data,fShift+5) &&
                        current>high(data, fShift-1) && current>high(data, fShift-2)){
                        foundUp = true
                        //                    update(1)
                    }
                }


                // find for 9 bars fractals

                if (!foundUp && (size - fShift - 1) >= 6){
                    if (current>=high(data, fShift+1) && current==high(data,fShift+2) && current>=high(data,fShift+3) &&
                        current==high(data, fShift+4) && current>high(data,fShift+5) && current>high(data,fShift+6) &&
                        current>high(data, fShift-1) && current>high(data, fShift-2)){
                        foundUp = true
                        //                    update(1)
                    }
                }

                /************************************************
                  * PROCESS FRACTAL DOWN
                  ***********************************************/

                var foundDown = false
                current = data(fShift).low

                if (current<low(data,fShift+1) && current<low(data,fShift+2) &&
                    current<low(data,fShift-1) && current<low(data,fShift-2)){
                    foundDown = true
                    //                update(0)
                }

                // find for 6 bars fractal
                if (!foundDown && (size - fShift - 1) >= 3){
                    if (current==low(data,fShift+1) && current<low(data,fShift+2) && current<low(data,fShift+3) &&
                        current<low(data,fShift-1) && current<low(data,fShift-2)){
                        foundDown = true
                        //                    update(0)
                    }
                }

                // find for 7 bars fractal
                if (!foundDown && (size - fShift - 1) >= 4){
                    if (current<=low(data,fShift+1) && current==low(data,fShift+2) && current<low(data,fShift+3) &&
                        current<low(data,fShift+4) &&
                        current<low(data,fShift-1) && current<low(data,fShift-2)){
                        foundDown = true
                        //                    update(0)
                    }
                }

                // find for 8 bars fractal
                if (!foundDown && (size - fShift - 1) >= 5){
                    if (current<=low(data,fShift+1) && current==low(data,fShift+2) && current==low(data,fShift+3) &&
                        current<low(data,fShift+4) && current<low(data,fShift+5) &&
                        current<low(data,fShift-1) && current<low(data,fShift-2)){
                        foundDown = true
                        //                    update(0)
                    }
                }

                // find for 9 bars fractal
                if (!foundDown && (size - fShift - 1) >= 6){
                    if (current<=low(data,fShift+1) && current==low(data,fShift+2) && current<=low(data,fShift+3) &&
                        current==low(data,fShift+4) && current<low(data,fShift+5) && current<low(data,fShift+6) &&
                        current<low(data,fShift-1) && current<low(data,fShift-2)){
                        foundDown = true
                        //                    update(0)
                    }
                }

                if (zigzagMapBuffer(fShift) == 0.0){
                    if (foundUp){
                        zigzagMapBuffer(fShift) = 1
                    }
                    if (foundDown){
                        zigzagMapBuffer(fShift) = 0
                    }
                    if (!foundUp && !foundDown){
                        zigzagMapBuffer(fShift) = -1
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

            shift = shift + 1
        }


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

}
