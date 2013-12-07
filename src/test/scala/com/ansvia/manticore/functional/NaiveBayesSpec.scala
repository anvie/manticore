package com.ansvia.manticore.functional

import org.specs2.Specification
import breeze.classify.{LogisticClassifier, NaiveBayes}
import breeze.data.Example
import breeze.linalg.Counter

/**
 * Author: robin
 * Date: 12/7/13
 * Time: 3:35 PM
 *
 */
class NaiveBayesSpec extends Specification {

    def is = "Naive bayes should" ^
            "calculate 4 digit correctly" ! trees.calculate4Digit ^
        end

    object trees {

        val dataUp = Array("0101","1101")
        val dataDown = Array("0100","1100")

        val data = dataUp.map(d => Example[String,Counter[String,Double]]("up", Counter.apply( (d,1.0) ))) ++
            dataDown.map(d => Example[String,Counter[String,Double]]("down", Counter.apply( (d,1.0) )))

        val x = new NaiveBayes.Trainer[String, String]()
        val c = x.train(data)

        def calculate4Digit = {
            c.classify(Counter( ("0101",1.0) )) must_== "up"
            c.classify(Counter( ("1101",1.0) )) must_== "up"
        }


    }
}
