package com.ansvia.manticore

import java.io.File
import com.ansvia.commons.logging.Slf4jLogger
import scala.io.Source

/**
 * Author: robin
 * Date: 10/29/13
 * Time: 10:53 PM
 *
 */

abstract class DataSource {
    def size:Long
    def foreach(func: (Int, Long) => Unit)
    def apply(i:Long):Int
    def indexedData:IndexedSeq[Int]
}


object Ignored extends Exception("ignored")

class InlineDataSource(data:Seq[Int]) extends DataSource {
    private val dataI = data.reverse.toIndexedSeq

    def size = dataI.length + 1

    def foreach(func: (Int, Long) => Unit) = {
        func(-1, 0)
        var i = 1L
        dataI.foreach { s =>
            func(s, i)
            i = i + 1
        }
    }

    def apply(i: Long) = {
        dataI(i.toInt)
    }


    def indexedData = dataI

    override def toString = "Inline"
}



/************************************************
 * CSV SOURCE
 ***********************************************/

class CsvDataSource(file:File,depth:Int) extends DataSource with Slf4jLogger {
    private lazy val _size = {
        val n = io.Source.fromFile(file).getLines().size
        if (depth == -1){
            n
        }else{
            if (n < depth)
                n
            else
                depth
        }
    }

    def size = _size + 1

    private lazy val _data = {

        var (prevBid, _) = (0.0, 0.0)

        var count = 1L

        io.Source.fromFile(file).getLines().slice(0, depth).toSeq.reverse
            .map { line =>

            print("    processing: " + count + ". " + line)

            val (bid, _) = parse(line)

            val bin = if (bid > prevBid)
                1
            else
                0

            println(" (" + bin + ")")

            prevBid = bid
//            prevAsk = ask

            count = count + 1

            bin
        }.reverse
    }

    def foreach(func: (Int, Long) => Unit) = {

        func(-1, 0)

        try {
            var idx = 1L
            _data.foreach { bin =>
                func(bin, idx)
                idx = idx + 1

//                if (idx > depth)
//                    throw Ignored
            }

        }catch{
            case Ignored =>

        }
        println("\r")
    }

    private def parse(line:String) = {
        val s = line.split(",")
        (s(2).toDouble, s(3).toDouble)
    }

    private lazy val _dataI = {
        _data.toIndexedSeq
    }

    def apply(i: Long) = _dataI(i.toInt)

    def indexedData = _dataI

    override def toString = "CSV: " + file.getAbsolutePath
}


/************************************************
 * Binary data source
 ***********************************************/
class BinaryDataSource(file:File) extends DataSource {

    private lazy val _data = {
        Source.fromFile(file).buffered.map { c =>
            if (c == 0x01)
                1
            else
                0
        }.toIndexedSeq
    }

    def size = _data.size + 1

    def foreach(func: (Int, Long) => Unit) = {
        func(-1, 0L)
        var idx = 1L
        _data.foreach { bin =>
            func(bin, idx)
            idx = idx + 1
        }
    }

    def apply(i: Long) = {
        _data(i.toInt)
    }

    def indexedData = {
        _data
    }

    override def toString = "(binary) " + file.getAbsolutePath
}