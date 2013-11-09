package com.ansvia.manticore

import java.io.File
import com.ansvia.commons.logging.Slf4jLogger
import scala.io.Source
import com.ansvia.manticore.DataSource.Chunk
import java.text.SimpleDateFormat
import scala.collection.immutable

/**
 * Author: robin
 * Date: 10/29/13
 * Time: 10:53 PM
 *
 */



abstract class DataSource {
    def size:Long
    def foreach(func: Chunk => Unit)
    def apply(i:Long):Chunk
    def indexedData:IndexedSeq[Chunk]
}

object DataSource {
    type Chunk = (Long, Int, Long)
}

object Ignored extends Exception("ignored")

class InlineDataSource(data:Seq[Int]) extends DataSource {
    private val dataI = data.toIndexedSeq

    def size = dataI.length + 1

    def foreach(func: Chunk => Unit) = {
        func(0L, -1, 0)
        var i = 1L
        dataI.foreach { s =>
            func(0L, s, i)
            i = i + 1
        }
    }

    def apply(i: Long) = {
        (0L, dataI(i.toInt), i)
    }


    def indexedData = {
        var count = -1L
        dataI.map{ x =>
            count = count + 1L
            (0L, x, count)
        }
    }

    override def toString = "Inline"
}



/************************************************
 * CSV SOURCE
 ***********************************************/

class CsvDataSource(file:File, depth:Int) extends DataSource with Slf4jLogger {
    
    def this(filePath:String, depth:Int){
        this(new File(filePath), depth)
    }
    
    
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

    private lazy val _data: Seq[Chunk] = {

        var prevBin = 0

        var count = 1L
        val nDepth = if (depth == -1)
            size.toInt
        else
            depth.toInt


        io.Source.fromFile(file).getLines().slice(0, nDepth).toSeq
            .map { line =>

//            print("    processing: " + count + ". " + line)

            val (ts, open, close) = parse(line)

            val bin = if (open > close)
                0
            else if (open == close)
                prevBin
            else
                1

//            println(" (" + bin + ")")

            prevBin = bin
//            prevAsk = ask

            count = count + 1

            (ts, bin, count)
        }
    }

    def foreach(func: Chunk => Unit) = {

        func(0L, -1, 0)

        try {
            var idx = 1L
            _data.foreach { case (ts,bin,_) =>
                func(ts,bin, idx)
                idx = idx + 1

//                if (idx > depth)
//                    throw Ignored
            }

        }catch{
            case Ignored =>

        }
//        println("\r")
    }

    def slice(from:Int, to:Int): immutable.IndexedSeq[Chunk] = {
        _dataI.slice(from, to)
    }

    private val dateTimeFormatter = new SimpleDateFormat("yyyy.MM.dd hh:mm")

    private def parse(line:String) = {
        val s = line.split(",")
        if (s.length == 7)
            (dateTimeFormatter.parse(s(0) + " " + s(1)).getTime, s(2).toDouble, s(5).toDouble)
        else
            (dateTimeFormatter.parse(s(0)).getTime, s(2).toDouble, s(3).toDouble)
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
        var idx = -1L
        Source.fromFile(file).buffered.map { c =>
            val bin = if (c == 0x01)
                1
            else
                0
            idx = idx + 1L
            (0L, bin, idx)
        }.toIndexedSeq
    }

    def size = _data.size + 1

    def foreach(func: Chunk => Unit) = {
        func(0L, -1, 0L)
        var idx = 1L
        _data.foreach { case (ts, bin, _) =>
            func(0L, bin, idx)
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