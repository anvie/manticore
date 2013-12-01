package com.ansvia.manticore.algo

/**
 * Author: robin
 * Date: 12/2/13
 * Time: 12:21 AM
 *
 */

class ManticoreException(msg:String) extends Exception(msg)

class Ignored extends ManticoreException("ignored")
