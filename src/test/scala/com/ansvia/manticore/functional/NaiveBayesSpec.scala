package com.ansvia.manticore.functional

import org.specs2.Specification
import breeze.classify.NaiveBayes
import breeze.data.Example
import breeze.linalg.Counter
import com.ansvia.manticore.algo.{Result, AI}
import com.ansvia.manticore.Direction

/**
 * Author: robin
 * Date: 12/7/13
 * Time: 3:35 PM
 *
 */
class NaiveBayesSpec extends Specification {

    def is = "Naive bayes should" ^
        p ^
            "calculate 4 digit correctly" ! trees.calculate4Digit ^
            "usable by AI" ! trees.usableAI ^
        end

    object trees {

        val dataUp = Array("0101","1101")
        val dataDown = Array("0100","1100")

        val data = dataUp.map(d => Example[String,Counter[String,Double]]("up", Counter.apply( (d,1.0) ))) ++
            dataDown.map(d => Example[String,Counter[String,Double]]("down", Counter.apply( (d,1.0) )))

        val x = new NaiveBayes.Trainer[String, String]()
        val c = x.train(data)

        val ai = new AI {
            def correctPrevious(result: Result){}
        }

        def calculate4Digit = {
            (c.classify(Counter( ("0101",1.0) )) must_== "up") and
                (c.classify(Counter( ("1101",1.0) )) must_== "up") and
                (c.classify(Counter( ("1100",1.0) )) must_== "down")
        }

        def usableAI = {
            ai.train("11101",Result(Direction.UP,0.0))
            ai.train("111011",Result(Direction.UP,0.0))
            ai.train("00100010",Result(Direction.DOWN,0.0))

            (ai.predict("11101") must_== Direction.UP) and
                (ai.predict("111011") must_== Direction.UP) and
                (ai.predict("00100010") must_== Direction.DOWN)
        }


    }
}
