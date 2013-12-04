package com.ansvia.manticore

import java.text.SimpleDateFormat

/**
 * Author: robin
 * Date: 12/4/13
 * Time: 9:27 PM
 *
 */
object Util {


    private val GENERIC_TIME_STYLE = """\d{4}\.\d{2}\.\d{2} \d{2}\:\d{2}""".r
    private val GENERIC_TIME_FORMAT = new SimpleDateFormat("yyyy.MM.dd HH:mm")
    private val DUKASCOPY_TIME_STYLE = """(\d{2})\.(\d{2})\.(\d{4}) (\d{2})\:(\d{2})\:(\d{2})\.\d*""".r
    private val DUKASCOPY_TIME_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

    def parseTime(timeStr:String) = {

        timeStr match {
            case DUKASCOPY_TIME_STYLE(day, month, year, hour, minute, second) => {
                DUKASCOPY_TIME_FORMAT.parse("%s.%s.%s %s:%s:%s".format(day, month, year, hour, minute, second))
            }
            case GENERIC_TIME_STYLE() => {
                GENERIC_TIME_FORMAT.parse(timeStr)
            }
        }

    }


}
