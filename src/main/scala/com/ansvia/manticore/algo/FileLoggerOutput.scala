package com.ansvia.manticore.algo

import java.io.{PrintWriter, FileOutputStream, File}

/**
 * Author: robin
 * Date: 12/6/13
 * Time: 3:44 PM
 *
 */
class FileLoggerOutput(path:String) {
    val f = new File(path)

    if (f.exists())
        f.delete()

    val fw = new FileOutputStream(f)
    val bw = new PrintWriter(fw)

    def println(text:String) = {
        bw.println(text)
        bw.flush()
    }

    def print(text:String) = {
        bw.print(text)
        bw.flush()
    }

    def close(){
        fw.close()
        bw.close()
    }

}
