package com.ansvia.manticore

import org.specs2.mutable.Specification
import java.util.Date

/**
 * Author: robin
 * Date: 11/9/13
 * Time: 12:22 PM
 *
 */
class CsvDataSourceSpec extends Specification {

    val result =
        """
          |idx: 19555, ts: Fri Nov 08 23:55:00 WIT 2013, value: 1
          |idx: 19556, ts: Fri Nov 08 23:56:00 WIT 2013, value: 1
          |idx: 19557, ts: Fri Nov 08 23:57:00 WIT 2013, value: 1
          |idx: 19558, ts: Fri Nov 08 23:58:00 WIT 2013, value: 0
          |idx: 19559, ts: Fri Nov 08 23:59:00 WIT 2013, value: 0
        """.stripMargin.split("\n").toSeq.filter(_.length > 2)

    "CsvDataSource for bin" should {
        "iterate bin value from csv file" in {
            val csvs = new CsvDataSource("data/EURUSD1.csv", -1)
            var i = 0
            csvs.slice(csvs.size.toInt - 6, csvs.size.toInt).foreach { case (ts, value, idx) =>
                val rv = "idx: " + idx + ", ts: " + new Date(ts).toString +  ", value: " + value
                println(rv + "   <==>   " + result(i))
                rv must_== result(i)
                i = i + 1
            }
        }
    }

}
