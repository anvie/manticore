package com.ansvia.manticore

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import java.io.ByteArrayInputStream

/**
 * Author: robin
 * Date: 11/7/13
 * Time: 11:47 PM
 *
 */
class CsvReaderSpec extends Specification {

    class Ctx extends Scope {
        val inlinedata =
            """
              |time,open,high,low,close,volumes
              |123,1.333,1.334,1.332,1.334,3.0
              |124,1.332,1.335,1.332,1.335,7.0
            """.stripMargin.trim
        val inlineReader = new CsvReader(new ByteArrayInputStream(inlinedata.getBytes))

        val realReader = new CsvReader("data/EURUSD60.csv")
    }

    "CsvReader" should {
        "read entire records correctly from inline data" in new Ctx {
           inlineReader.nextRecord() must beEqualTo(Record(0,"123",1.333,1.334,1.332,1.334,3.0))
           inlineReader.nextRecord() must beEqualTo(Record(1,"124",1.332,1.335,1.332,1.335,7.0))
        }
        "read entire records correctly from real data EURUSD60.csv" in new Ctx {
            realReader.nextRecord() must beEqualTo(Record(0,"2003.05.12 16:00",1.15590,1.15730,1.15490,1.15580,652.0))
            realReader.nextRecord() must beEqualTo(Record(1,"2003.05.12 17:00",1.15590,1.15700,1.15530,1.15610,595))
        }
    }
}
