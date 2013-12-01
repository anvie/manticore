package com.ansvia.manticore.algo

/**
 * Author: robin
 * Date: 12/2/13
 * Time: 12:04 AM
 *
 */
abstract class ManticoreAlgo {

    /**
     * Manticore algo result
     * @param direction swing direction @see [[com.ansvia.manticore.Direction]]
     * @param pips pips information if any.
     */
    case class Result(direction:Int, pips:Double)

    def calculate(pos:Int):Result

    def lastResult:Result
}
