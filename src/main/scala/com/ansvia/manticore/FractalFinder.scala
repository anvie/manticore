package com.ansvia.manticore

import org.apache.commons.io.FileUtils
import java.io.File
import com.ansvia.commons.logging.Slf4jLogger


object FractalFinder extends Slf4jLogger {



    def usage(){
        println("Usage:\n")
        println("      ./ffind [CSV-FILE]")
        sys.exit(2)
    }


    def main(args:Array[String]){


        import DataModes._

        val fileDataPath = args(0)


        val mode = FileUtils.getExtension(fileDataPath) match {
            case "bin" => BINARY
            case "csv" => CSV
            case _ =>
                println("  [ERROR] Unknown file input format " + fileDataPath)
                sys.exit(4)
        }

        val source:DataSource = {
            mode match {
                case BINARY =>
                    new BinaryDataSource(new File(fileDataPath))
                case CSV =>

                    // convert dulu ke binary

                    info("converting csv to bin...")

                    CsvToBin.convert(new File(fileDataPath))

                    val binFilePath = FileUtils.removeExtension(fileDataPath) + ".bin"
                    new BinaryDataSource(new File(binFilePath))
            }
        }
        println(" + data source: " + source + "\n")






    }

}
