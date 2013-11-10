package com.ansvia.manticore

/**
 * Author: robin
 * Date: 11/10/13
 * Time: 12:56 AM
 *
 */
class ZigzagFinder(data:Array[Record], depth:Int=12, deviation:Int=5, backstep:Int=8) {

    val size = data.length
    private var lowMapBuffer = new Array[Double](size + 1)
    private var highMapBuffer = new Array[Double](size + 1)
    private var zigzagMapBuffer = new Array[Double](size + 1)

    def instrPips = 0.0

    def process() = {

        var lastLow = 0.0
        var lastHigh = 0.0
        var curLow = 0.0
        var curHigh = 0.0
        var lookFor = 0
        var idx = 0

        var shift = size - 1


        var lastHighPos = 0
        var lastLowPos = 0

        while(shift > 0){

            idx = idx + 1

            // cari lowest value
            var v = lowest(shift)
//
//            if (v == lastLow)
//                v = 0.0
//            else {
//                lastLow = v
//                if ((data(shift).low - v) > (deviation * instrPips)){
//                    v = 0.0
//                }else{
//                    for (back <- 1 to backstep){
//                        if (shift - back > 0){
//                            val res = lowMapBuffer(shift-back)
//                            if (res != 0.0 && res > v){
//                                lowMapBuffer(shift-back)=0.0
//                            }
//                        }
//                    }
//                }
//            }

            if (data(shift).low==v){
                lastLow = v
                lowMapBuffer(shift) = v
            }else{
                lowMapBuffer(shift) = 0.0
            }

            // cari highest value
            v = highest(shift)
//            if (v == lastHigh)
//                v = 0.0
//            else {
//                lastHigh = v
//                if ((v - data(shift).high) > (deviation * instrPips))
//                    v = 0.0
//                else {
//                    for (back <- 1 to backstep){
////                        println("back: " + back)
//                        if (shift - back > 0){
//                            val res = highMapBuffer(shift-back)
//                            if (res != 0.0 && res < v){
//                                highMapBuffer(shift-back)=0.0
//                            }
//                        }
//                    }
//                }
//            }

            if (data(shift).high==v){
                if (data(shift).high == data(shift-1).high)
                    highMapBuffer(shift) = 0.0
                else
                    highMapBuffer(shift) = v
            }else{
                highMapBuffer(shift) = 0.0
            }

            shift = shift - 1

        }


        // final cutting
        if (lookFor == 0){
            lastLow = 0.0
            lastHigh = 0.0
        }else{
            lastLow = curLow
            lastHigh = curHigh
        }

        shift = size - 1
        idx = 0

        while(shift >= 0){
            var res = 0.0
            idx = idx + 1
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

            shift = shift - 1
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

    def getZigZagBuffer = zigzagMapBuffer

}
