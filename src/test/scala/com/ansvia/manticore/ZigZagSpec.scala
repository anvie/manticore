package com.ansvia.manticore

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * Author: robin
 * Date: 11/10/13
 * Time: 11:49 AM
 *
 */
class ZigZagSpec extends Specification {

    class Ctx(path:String) extends Scope {
        val csvr = new CsvReader(path)
    }

    "Zigzag finder" should {
        "find zigzag basic" in new Ctx("data/EURUSD1.csv") {

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


            val data = csvr.toArray
//            val reversedData = data.reverse
//            val size = reversedData.size
            val zzf = new ZigZagFinder(data)
            val zb = zzf.process().getZigZagBuffer.filter(x => x != 0.0 && x != 1.0 && x != -1.0)
            zb.slice(zb.length-expectedDatas.length,zb.length).zipWithIndex.foreach { case (d, i) =>
//                if (d > 0.0){
                    val time = data(i).time
                    val str = "%s => %s".format(time, d)
                    println(str)
                    str must_== expectedDatas(i)
//                }
            }

        }
        "find zigzag fractals" in new Ctx("data/EURUSD1.csv"){
            val expectedDatas =
                """
                  |2013.11.08 23:35 => 0.0
                  |2013.11.08 23:36 => 1.0
                  |2013.11.08 23:37 => -
                  |2013.11.08 23:38 => -
                  |2013.11.08 23:39 => -
                  |2013.11.08 23:40 => -
                  |2013.11.08 23:41 => -
                  |2013.11.08 23:42 => 0.0
                  |2013.11.08 23:43 => -
                  |2013.11.08 23:44 => -
                  |2013.11.08 23:45 => 1.33678
                  |2013.11.08 23:46 => -
                  |2013.11.08 23:47 => -
                  |2013.11.08 23:48 => -
                  |2013.11.08 23:49 => -
                  |2013.11.08 23:50 => -
                  |2013.11.08 23:51 => 0.0
                  |2013.11.08 23:52 => -
                  |2013.11.08 23:53 => -
                  |2013.11.08 23:54 => -
                  |2013.11.08 23:55 => 1.33612
                  |2013.11.08 23:56 => -
                  |2013.11.08 23:57 => -
                  |2013.11.08 23:58 => 1.33682
                  |2013.11.08 23:59 => -
                """.stripMargin
            val data = csvr.toArray
            val zzf = new ZigZagFinder(data)
            val zb = zzf.process().getZigZagBuffer
            zb.slice(zb.length-expectedDatas.length,zb.length).zipWithIndex.foreach { case (d,i) =>
                val time = data(i).time
                val x = if (d == -1) "-" else d.toString
                val str = "%s => %s".format(time, x)
//                if (d > -1.0){
                println(str)
//                }
            }
        }
    }
}
