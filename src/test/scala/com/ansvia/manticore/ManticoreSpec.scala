package com.ansvia.manticore

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * Author: robin
 * Date: 10/30/13
 * Time: 9:26 PM
 *
 */
class ManticoreSpec extends Specification {


    class Ctx extends Scope {
        val data = Seq(
            0,0,1,1,1,1,0,0,0,1,1,1,
            0,1,1,1,0,0,1,1
        )

        val source = new InlineDataSource(data)
        val result = Manticore.process(source, 4)
        println(result)
    }

    "Manticore engine" should {
        "calculate positives negatives correctly" in new Ctx {
            result.positives must_== 8
            result.negatives must_== 4
            math.floor(result.upPercent) must_== 66
            math.floor(result.downPercent) must_== 33
        }
//        "calculate up percent" in new Ctx {
//            result.upPercent must_== 66
//            result.downPercent must_== 44
//        }
    }

}
