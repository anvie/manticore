package com.ansvia.manticore

import scala.util.control.Breaks._
import com.ansvia.commons.logging.Slf4jLogger
import java.io.File
import org.apache.commons.io.FileUtils
import scala.actors.threadpool.AtomicInteger


object Manticore extends Slf4jLogger {

    type FourSeq = Seq[Seq[Int]]
    type DNA = Seq[(Int, Long)]
    type FourSeqI = Seq[DNA]

    def padToFourSeq(ds:DataSource):FourSeqI = {
        var rv = Seq.newBuilder[Seq[(Int, Long)]]
        var buff = Seq.newBuilder[(Int, Long)]
        var buffCount = 0
        var jump = 1
        var count = 0
//        val reversedData = data.reverse.zipWithIndex
        val dataCount = ds.size
        var done = dataCount < 1
        while(!done){
            breakable {
                ds.foreach { case (d, i) =>

                    if (i % jump == 0){
                        if (buffCount < 4){
                            val index = dataCount - i
                            if (index < 1){
                                done = true
                                break
                            }
                            buff ++= Seq((d, index))
                            buffCount = buffCount + 1
                        }else if (buffCount == 4) {

//                            debug("buff: " + buff.result())

                            rv += buff.result().reverse

                            jump = jump + 1
                            buffCount = 0
                            buff.clear()

                            count = count + 1

                            if (count >= ds.size)
                                done = true

                            break
                        }else{
                            debug("last 4seq: " + buff.result())
                            buff.clear()
                            debug("no more FourSeq")
                            break
                        }
                    }
                }
                done = true
                // update last data
                if (buff.result().length == 4)
                    rv += buff.result().reverse
            }
        }
        rv.result()
    }

    def prettyPrint(fs:FourSeqI){
        fs.foreach { f =>
//            debug("f: " + f)
            val lastF = if (f(3)._1 == -1)
                "X|%02d".format(f(3)._2)
            else
                f(3)
            println("   %d|%02d %d|%02d %d|%02d %s".format(f(0)._1, f(0)._2, f(1)._1, f(1)._2,f(2)._1,f(2)._2, lastF))
        }
        println("")
    }
    
    case class ChromosomeFinder(dna:DNA, index:Int, data:IndexedSeq[Int],
                                positivePattern:Seq[Int], negativePattern:Seq[Int],
                                positives:AtomicInteger, negatives:AtomicInteger,
                                chromosomes:AtomicInteger) extends Thread {
        override def run(){

            var i1 = dna(3)._2.toInt
            var i2 = dna(2)._2.toInt
            var i3 = dna(1)._2.toInt
            var i4 = dna(0)._2.toInt

            if (i4 > 1){
                println("       (thread-%s) processing DNA #%d  %d|%02d %d|%02d %d|%02d X|%02d".format(
                    Thread.currentThread().getId, index,
                    dna(0)._1,dna(0)._2,dna(1)._1,dna(1)._2,dna(2)._1,dna(2)._2,dna(3)._2))
            }

            while(i4 > 1){

                i4 = i4 - 1
                i3 = i3 - 1
                i2 = i2 - 1
                i1 = i1 - 1

                val d4 = data(i4-1)
                val d3 = data(i3-1)
                val d2 = data(i2-1)
                val d1 = data(i1-1)

                //                    print("   %02d %02d %02d %02d".format(d4, d3, d2, d1))

                val chrom = Seq(d4, d3, d2, d1)
                if (positivePattern == chrom){
                    positives.getAndIncrement
                    //                        println(" +")
                }else if (negativePattern == chrom){
                    negatives.getAndIncrement
                    //                        println(" -")
                }else{
                    //                        println("")
                }

                chromosomes.incrementAndGet()
            }

//            (positives.get(), negatives.get())
        }
    }

    def breakDown(fss:FourSeqI, data:IndexedSeq[Int]) = {

        val fs = fss(0)
        val positivePattern = Seq(fs(0)._1,fs(1)._1,fs(2)._1,1)
        val negativePattern = Seq(fs(0)._1,fs(1)._1,fs(2)._1,0)

        val positives = new AtomicInteger(0)
        val negatives = new AtomicInteger(0)
        val chromosomes = new AtomicInteger(0)
        var threads = Seq.empty[ChromosomeFinder]

        val index = new AtomicInteger(0)

        val tsBefore = System.currentTimeMillis()

        fss.foreach { fs =>

            threads :+= ChromosomeFinder(fs, index.incrementAndGet(), data, positivePattern,
                negativePattern, positives, negatives, chromosomes)

        }

        // wait until all calculation done
        threads.foreach(_.start())
        println("    + waiting for %d background distributed calculation...".format(fss.size))
        threads.foreach(_.join())
        println("    + calculation completed, took %sms".format(System.currentTimeMillis() - tsBefore))

        (positives.get(), negatives.get(), chromosomes.get())
    }



    def main(args:Array[String]){

//        val data = Seq(
//            0,0,1,1,1,1,0,
//            0,0,1,1,1,0,1,
//            1,1,0,0,1,1,-1
//        )

        val fileDataPath = args(0)
        val depth = try {
            args(1).toInt
        }
        catch {
            case _ => -1
        }
        
        val fileDataF = new File(fileDataPath)
        if (!fileDataF.exists()){
            println(" [ERROR] File not found " + fileDataPath)
            sys.exit(3)
        }
        
        import DataModes._
        
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
                    new CsvDataSource(new File(fileDataPath), depth)
            }
        }
        println(" + data source: " + source + "\n")

        println(" + padding...\n")

//        val rv = padToFourSeq(new InlineDataSource(data))
        val rv = padToFourSeq(source)
        println("\r")
        prettyPrint(rv)
        println("")

        println(" + breaking down...\n")
        val (positives, negatives, chromosomes) = breakDown(rv, source.indexedData)

        println("")
        println(" + result:\n")
        println("   Processed " + rv.length + " DNA and " + chromosomes + " chromosomes.")
        println("   Positives: %d, Negatives: %d".format(positives, negatives))
        println("   Probability:")
        println("               \u25B2 " +  ((positives * 100) / (positives + negatives)) + "%")
        println("               \u25BC " +  ((negatives * 100) / (positives + negatives)) + "%")
        println("")

    }

}

object DataModes {
    val BINARY = 1
    val CSV = 2
}