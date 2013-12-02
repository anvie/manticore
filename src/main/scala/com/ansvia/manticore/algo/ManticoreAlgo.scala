package com.ansvia.manticore.algo

import com.ansvia.manticore.Leg

/**
 * Author: robin
 * Date: 12/2/13
 * Time: 12:04 AM
 *
 */

/**
 * Manticore algo result
 * @param direction swing direction @see [[com.ansvia.manticore.Direction]]
 * @param pips pips information if any.
 */
case class Result(direction:Int, pips:Double)


abstract class ManticoreAlgo {

    def calculate(posTime:String):Result

    def lastResult:Result
}

trait AI {
    def train(posTime:String, result:Result)
    def guess(posTime:String):Option[Result]
    def markWrong()
}

