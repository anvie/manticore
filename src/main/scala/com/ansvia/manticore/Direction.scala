package com.ansvia.manticore

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 6:46 PM
 *
 */
object Direction {
    val UP = 1
    val DOWN = 0
    val NEUTRAL = -1

    def toStr(d:Int) = {
        d match {
            case UP => "up"
            case DOWN => "down"
            case NEUTRAL => "-"
        }
    }

}
