package com.ansvia.manticore

import java.text.SimpleDateFormat

/**
 * Author: robin
 * Date: 12/2/13
 * Time: 12:04 AM
 *
 */
case class DataGenerator(data:IndexedSeq[Record], startTime:String="", endTime:String=""){

    val startTs = if (startTime.length > 0)
            Util.parseTime(startTime).getTime
        else
            data(0).timestamp
    val endTs = if (endTime.length > 0)
            Util.parseTime(endTime).getTime
        else
            data(data.length-1).timestamp

    /**
     * contains data that has been sliced out
     * using startTime and endTime.
     */
    lazy val chunkedData = {
        if (startTime.length > 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp > startTs && d.timestamp < endTs
            }
        }else if (startTime.length > 0 && endTime.length == 0){
            data.filter { d =>
                d.timestamp > startTs
            }
        }else if (startTime.length == 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp < endTs
            }
        }else{
            data
        }
    }
    lazy val zzfRaw = {
//        println("calculating zigzag (raw)...")
        new ZigZagFinder(data, 13, 8, 5).process()
    }
    lazy val zzLegsRaw = zzfRaw.getLegs
    lazy val zzfChunked = new ZigZagFinder(chunkedData, 13, 8, 5).process()
    lazy val zzLegsChunked = zzfChunked.getLegs

    lazy val lastLegRaw = zzLegsRaw(zzLegsRaw.length - 1)
    lazy val lastLegChunked = zzLegsChunked(zzLegsChunked.length - 1)

    /**
     * Unidentified leg a.k.a uncompleted leg (chunked).
     */
    lazy val uLeg = {
        val trailingData = chunkedData.filter(_.timestamp > lastLegChunked.timestamp)

        var fractals = FractalFinder.find(trailingData)
        if (fractals(fractals.length-1).isInstanceOf[Fractal]){
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
