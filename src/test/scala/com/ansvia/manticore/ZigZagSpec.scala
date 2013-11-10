package com.ansvia.manticore

import org.specs2.mutable.Specification

/**
 * Author: robin
 * Date: 11/10/13
 * Time: 11:49 AM
 *
 */
class ZigZagSpec extends Specification {

    "Zigzag finder" should {
        "find zigzag basic" in {

            val csvr = new CsvReader("data/EURUSD1.csv")
            val data = csvr.toArray
            val zzf = new ZigzagFinder(data)
            zzf.process().getZigZagBuffer.foreach { d =>
                println(d)
            }

        }
    }
}
