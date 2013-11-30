//package com.ansvia.manticore
//
//import org.specs2.mutable.Specification
//
///**
// * Author: robin
// * Date: 11/30/13
// * Time: 3:38 PM
// *
// */
//class JaccardCoefficientSpec extends Specification {
//
//
//    "JaccardCoefficient" should {
//        "111 vs 000 result must be 0.0" in {
//
//            JaccardCoefficient.calculate(Array[Byte](0x01,0x01,0x01), Array[Byte](0x00,0x00,0x00)) must_== 0.00
//
//        }
//        "111 vs 001 result must be 0.0" in {
//
//            JaccardCoefficient.calculate(Array[Byte](0x01,0x01,0x01), Array[Byte](0x00,0x00,0x01)) must_== 0.33
//
//        }
//    }
//}
