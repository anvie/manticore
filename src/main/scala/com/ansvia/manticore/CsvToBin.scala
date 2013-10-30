package com.ansvia.manticore

import java.io._
import com.ansvia.commons.logging.Slf4jLogger
import scala.io.Source
import sun.misc.IOUtils

/**
 * Author: robin
 * Date: 10/30/13
 * Time: 1:49 PM
 *
 * Converter csv to binary file.
 */
object CsvToBin extends Slf4jLogger {

    def usage(){
        println("Usage:\n")
        println("      ./csv2bin [CSV-FILE]")
        sys.exit(2)
    }

    def main(args: Array[String]) {

        if (args.length < 1)
            usage()

        val filePath = args(0)
        val file = new File(filePath)

        try {
            if (!file.exists())
                throw new FileNotFoundException("File not found " + file.getAbsolutePath)

            val lines = Source.fromFile(file).getLines()

            // skip first line (header)
            lines.next()

//            var prevClose = 0.0
            var bytes = Seq.newBuilder[Byte]
            var prevBin = 0

            while(lines.hasNext){
                val line = lines.next()
                val s = line.split(",")
                val open = s(1).toDouble
                val close = s(4).toDouble
                val bin = if (close > open){
                    1
                } else if (close == open) {
                    prevBin
                } else {
                    0
                }
                prevBin = bin

//                prevClose = close

                bytes += bin.toByte
            }

            val fName = org.apache.commons.io.FileUtils.removeExtension(file.getAbsolutePath) + ".bin"

            val fileOut = new File(fName)
            if (fileOut.exists())
                fileOut.delete()
            val fileOutW = new FileOutputStream(fileOut)
            fileOutW.write(bytes.result().reverse.toArray)
            fileOutW.close()

            println("  + out: " + fName)
        }
        catch {
            case e:Exception =>
                error(e.getMessage)
        }

    }
}
