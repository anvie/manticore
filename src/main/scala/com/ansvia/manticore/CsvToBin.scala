package com.ansvia.manticore

import java.io.{BufferedWriter, FileWriter, FileNotFoundException, File}
import com.ansvia.commons.logging.Slf4jLogger
import scala.io.Source

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

            var prevClose = 0.0
            var bytes = Seq.newBuilder[Byte]

            while(lines.hasNext){
                val line = lines.next()
                val s = line.split(",")
                val close = s(4).toDouble
                val bin = if (close > prevClose) 1 else 0
                prevClose = close

                bytes += bin.toByte
            }

            val fName = org.apache.commons.io.FileUtils.removeExtension(file.getAbsolutePath) + ".bin"

            val fileOut = new FileWriter(fName)
            val fileOutW = new BufferedWriter(fileOut)
            fileOutW.write(bytes.result().reverse.map(_.toChar).toArray)
            fileOutW.close()

            println("  + out: " + fName)
        }
        catch {
            case e:Exception =>
                error(e.getMessage)
        }

    }
}
