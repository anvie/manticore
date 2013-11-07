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
        val data =
            """
              |time,open,high,low,close,volumes
              |123,1.333,1.334,1.332,1.334,3.0
              |124,1.332,1.335,1.332,1.335,7.0
            """.stripMargin.trim
        val reader = new CsvReader(new ByteArrayInputStream(data.getBytes))
    }

    "CsvReader" should {
        "read entire records correctly" in new Ctx {
           reader.nextRecord() must beEqualTo(Record(0,"123",1.333,1.334,1.332,1.334,3.0))
           reader.nextRecord() must beEqualTo(Record(1,"124",1.332,1.335,1.332,1.335,7.0))
        }
    }
}
