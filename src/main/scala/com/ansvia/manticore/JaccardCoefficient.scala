package com.ansvia.manticore

/**
 * Author: robin
 * Date: 11/30/13
 * Time: 3:35 PM
 *
 */
object JaccardCoefficient {


    def calculate(a:Array[Byte], b:Array[Byte]) = {

        val all = a ++ b
        val uniq = all.distinct

//        val exists = a.filter(b.contains)

        uniq.size / all.size
    }


}
