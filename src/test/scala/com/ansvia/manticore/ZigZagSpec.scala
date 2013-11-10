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

            val expectedDatas =
                """2013.10.22 07:22 => 1.33702
                  |2013.10.22 07:23 => 1.33572
                  |2013.10.22 07:24 => 1.33661
                  |2013.10.22 07:25 => 1.33559
                  |2013.10.22 07:26 => 1.33616
                  |2013.10.22 07:27 => 1.33508
                  |2013.10.22 07:28 => 1.33569
                  |2013.10.22 07:29 => 1.33518
                  |2013.10.22 07:30 => 1.33668
                  |2013.10.22 07:31 => 1.33575
                  |2013.10.22 07:35 => 1.33722
                  |2013.10.22 07:36 => 1.33636
                  |2013.10.22 07:37 => 1.33678
                  |2013.10.22 07:38 => 1.33612
                  |2013.10.22 07:39 => 1.33682
                """.stripMargin.trim.split("\n")

            val csvr = new CsvReader("data/EURUSD1.csv")
            val data = csvr.toArray
//            val reversedData = data.reverse
//            val size = reversedData.size
            val zzf = new ZigzagFinder(data)
            val zb = zzf.process().getZigZagBuffer.filter(x => x > 0.0)
            zb.slice(zb.length-expectedDatas.length,zb.length).zipWithIndex.foreach { case (d, i) =>
//                if (d > 0.0){
                    val time = data(i).time
                    val str = "%s => %s".format(time, d)
                    println(str)
                    str must_== expectedDatas(i)
//                }
            }

        }
    }
}
