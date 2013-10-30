package com.ansvia.manticore

import org.specs2.mutable.Specification
import java.io.{FileInputStream, File}

/**
 * Author: robin
 * Date: 10/30/13
 * Time: 10:45 PM
 *
 */
class CsvToBinSpec extends Specification {

    "csv2bin" should {
        "convert csv file to binary correctly" in {

            val binFile = new File("data/basic.bin")

            if (binFile.exists())
                binFile.delete()

            CsvToBin.convert(new File("data/basic.csv"))

            "create bin file" in {
                binFile.exists must_== true
            }

            "got expected size (20 bytes)" in {
                val fis = new FileInputStream(binFile)
                fis.getChannel.size() must_== 20
            }

        }
    }
}
