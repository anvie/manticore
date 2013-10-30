package com.ansvia.manticore

import scala.util.control.Breaks._
import com.ansvia.commons.logging.Slf4jLogger
import java.io.File
import org.apache.commons.io.FileUtils
import scala.actors.threadpool.AtomicInteger
import com.ansvia.manticore.Manticore.DNAS


case class CalculationResult(dnas:DNAS, positives:Int, negatives:Int, chromosomes:Int) {
    def upPercent = {
        val total = positives + negatives
        if (total == 0)
            0
        else
            (positives * 100) / total
    }

    def downPercent = {
        val total = positives + negatives
        if (total == 0)
            0
        else
            (negatives * 100) / total
    }

    override def toString = "Result[+%d,-%d,*:%d]".format(positives, negatives, chromosomes)

}



object Manticore extends Slf4jLogger {

    type FourSeq = Seq[Seq[Int]]
    type DNA = Seq[(Int, Long)]
    type DNAS = Seq[DNA]

    def getDnas(ds:DataSource, len:Int):DNAS = {
        var rv = Seq.newBuilder[Seq[(Int, Long)]]
        var buff = Seq.newBuilder[(Int, Long)]
        var buffCount = 0
        var jump = 1
        var count = 0
        val dataCount = ds.size
        var done = dataCount < 1
        while(!done){
            breakable {
                ds.foreach { case (d, i) =>

                    if (i % jump == 0){
                        if (buffCount < len){
                            val index = dataCount - i
                            if (index < 1){
                                done = true
                                break
                            }
                            buff ++= Seq((d, index))
                            buffCount = buffCount + 1
                        }else if (buffCount == len) {

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
                if (buff.result().length == len)
                    rv += buff.result().reverse
            }
        }
        rv.result()
    }
    
    def process(ds:DataSource, len:Int):CalculationResult = {
        val dnas = getDnas(ds, len)
        val (post, neg, chrom) = breakDown(dnas, ds.indexedData)
        CalculationResult(dnas, post, neg, chrom)
    }

    def prettyPrint(fs:DNAS){
        fs.foreach { f =>
//            debug("f: " + f)
            val lastF = if (f(3)._1 == -1)
                "X|%02d".format(f(3)._2)
            else
                f(3)
            println("   %d(%02d) %d(%02d) %d(%02d) %s".format(f(0)._1, f(0)._2, f(1)._1, f(1)._2,f(2)._1,f(2)._2, lastF))
        }
        println("")
    }



    def breakDown(dnas:DNAS, data:IndexedSeq[Int]) = {

        val dna = dnas.head

//        val positivePattern = Seq(fs(0)._1,fs(1)._1,fs(2)._1,1)
//        val negativePattern = Seq(fs(0)._1,fs(1)._1,fs(2)._1,0)
        val positivePattern = dna.slice(0,dna.length-1).map(_._1) ++ Seq(1)
        val negativePattern = dna.slice(0,dna.length-1).map(_._1) ++ Seq(0)

        val positives = new AtomicInteger(0)
        val negatives = new AtomicInteger(0)
        val chromosomes = new AtomicInteger(0)
        var threads = Seq.empty[ChromosomeFinder]

        val index = new AtomicInteger(0)

        val tsBefore = System.currentTimeMillis()

        dnas.foreach { dna =>

            threads :+= new NonBlockingChromosomeFinder(dna, index.incrementAndGet(), data, positivePattern,
                negativePattern, positives, negatives, chromosomes)

        }

        // wait until all calculation done
        threads.foreach(_.start())
        println("    + waiting for %d background(s) calculation...".format(dnas.size))
        threads.foreach(_.join())
        println("    + calculation completed, took %sms".format(System.currentTimeMillis() - tsBefore))

        (positives.get(), negatives.get(), chromosomes.get())
    }



    def main(args:Array[String]){

//        val inlineData = Seq(
//            0,0,1,1,1,1,0,
//            0,0,1,1,1,0,1,
//            1,1,0,0,1,1,-1
//        )

        val fileDataPath = args(0)
        val len = try {
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
//                INLINE
                println("  [ERROR] Unknown file input format " + fileDataPath)
                sys.exit(4)
        }

        val source:DataSource = {
            mode match {
                case BINARY =>
                    new BinaryDataSource(new File(fileDataPath))
                case CSV =>
                    new CsvDataSource(new File(fileDataPath), len)
//                case _ =>
//                    new InlineDataSource(inlineData)
            }
        }
        println(" + data source: " + source + "\n")

//        println(" + padding...\n")
//
////        val rv = padToFourSeq(new InlineDataSource(data))
//        val rv = getDnas(source)
//        println("\r")
//        prettyPrint(rv)
//        println("")
//
//        println(" + breaking down...\n")
//        val (positives, negatives, chromosomes) = breakDown(rv, source.indexedData)

        println(" + calculating...\n")

        val result = process(source, len)

        println("")
        println(" + result:\n")
        val pattern = {
            val dna = result.dnas.head
            dna.map(x => "%s(%d)".format( (if (x._1 == -1) "X" else x._1) , x._2))
                .reduceLeftOption(_ + " " + _).getOrElse("")
        }
        println("   Pattern: " + pattern)
        println("   Processed " + result.dnas.length + " DNA and " + result.chromosomes + " Chromosomes.")
        println("   Positives: %d, Negatives: %d".format(result.positives, result.negatives))
        println("   Probability:")
        println("               \u25B2 " + result.upPercent + "%")
        println("               \u25BC " + result.downPercent + "%")
        println("")

    }

}

object DataModes {
    val BINARY = 1
    val CSV = 2
    val INLINE = 3
}