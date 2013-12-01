package com.ansvia.manticore.tester

import com.ansvia.manticore._
import java.text.SimpleDateFormat
import com.ansvia.manticore.Record

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 5:41 PM
 *
 */
class AlgoTester(dataGen:DataGenerator, algo:ManticoreAlgo) {

    case class TesterResult(var passed:Int, var missed:Int){
        def printSummary() = {
            val accuracy = math.floor((passed * 100).toDouble / (passed + missed).toDouble)
            println(
                """
                  |Summary:
                  |-----------
                  |   Data size: %d
                  |   Start time: %s
                  |   End time: %s
                  |   Result: %d passed and %d missed.
                  |   Accuracy level: %f %%
                """.stripMargin.format(dataGen.data.size, dataGen.startTime, dataGen.endTime,
                    passed, missed, accuracy)
            )
        }

    }

    lazy val legIterator = dataGen.zzLegsChunked.toIterator


    def play():TesterResult = {

        var curPos = 0

        var passes = 0
        var misses = 0

        println("testing...")

        while(legIterator.hasNext){

            val leg = legIterator.next()

//            // fix offset
//            while (leg.timestamp < dataGen.chunkedData(curPos).timestamp){
//                curPos = curPos + 1
//            }

//            curPos = curPos + leg.barCount


            for (i <- 0 to leg.barCount - 1){
                val direction = algo.calculate(curPos + i).direction
                if (direction == leg.direction){
                    passes = passes + 1
                    print(".")
                }else if (direction == Direction.NEUTRAL){
                    print("_")
                }else{
                    misses = misses + 1
                    print("x")
                }
            }
            println("")

            curPos = curPos + leg.barCount

        }

        TesterResult(passes, misses)
    }


}





abstract class ManticoreAlgo {

    /**
     * Manticore algo result
     * @param direction swing direction @see [[com.ansvia.manticore.Direction]]
     * @param pips pips information if any.
     */
    case class Result(direction:Int, pips:Double)

    def calculate(pos:Int):Result

}

case class DataGenerator(data:IndexedSeq[Record], startTime:String="", endTime:String=""){
    private val formatter = new SimpleDateFormat("yyyy.MM.dd HH:mm")
    val startTs = if (startTime.length > 0)
            formatter.parse(startTime).getTime
        else
            0L
    val endTs = if (endTime.length > 0)
            formatter.parse(endTime).getTime
        else
            0L

    /**
     * contains data that has been sliced out
     * using startTime and endTime.
     */
    lazy val chunkedData = {
        if (startTime.length > 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp > startTs && d.timestamp < endTs
            }
        }else if (startTime.length > 0 && endTime.length == 0){
            data.filter { d =>
                d.timestamp > startTs
            }
        }else if (startTime.length == 0 && endTime.length > 0){
            data.filter { d =>
                d.timestamp < endTs
            }
        }else{
            data
        }
    }
    lazy val zzfRaw = new ZigZagFinder(data, 13, 8, 5)
    lazy val zzLegsRaw = zzfRaw.getLegs
    lazy val zzfChunked = new ZigZagFinder(chunkedData, 13, 8, 5)
    lazy val zzLegsChunked = zzfChunked.getLegs

    lazy val lastLegRaw = zzLegsRaw(zzLegsRaw.length - 1)
    lazy val lastLegChunked = zzLegsChunked(zzLegsChunked.length - 1)

    /**
     * Unidentified leg a.k.a uncompleted leg (chunked).
     */
    lazy val uLeg = {
        val trailingData = chunkedData.filter(_.timestamp > lastLegChunked.timestamp)

        var fractals = FractalFinder.find(trailingData)
        if (fractals(fractals.length-1).isInstanceOf[Fractal]){
            fractals = fractals.slice(0, fractals.length-2)
        }

        val fractalPattern = fractals.filter(_.isInstanceOf[Fractal])
            .map(_.asInstanceOf[Fractal]).map(_.pos)
        val barPattern = trailingData.map(_.bit)
        val fractalCount = fractalPattern.length
        val barCount = barPattern.length

        Leg("-", fractalCount, barCount, fractalPattern.map(_.toByte), barPattern.map(_.toByte).toArray, 0.0)
    }

}



object AlgoTester {

    val availableAlgos = Seq("MTH3", "FRAC1")

    def showUsage(){
        println("Usage: \n" +
            "AlgoTester [ALGO-NAME] [CSV-FILE] [START-TIME] [END-TIME]")
        sys.exit(2)
    }

    def main(args: Array[String]) {

        if (args.length < 3){
            showUsage()
        }

        val algoName = args(0)
        val csvFilePath = args(1)
        val startTime = if (args.length > 2) args(2) else ""
        val endTime = if (args.length > 3) args(3) else ""

        if (!availableAlgos.map(_.toLowerCase).contains(algoName.toLowerCase)){
            sys.error("No algo name: " + algoName)
            sys.error("Available algos: " + availableAlgos.mkString(", "))
            sys.exit(3)
        }

        println("Setup:")
        println("   algo name: " + algoName)
        println("   csv file: " + csvFilePath)
        println("   start time: " + startTime)
        println("   end time: " + endTime)
        println("")
        Console.readLine("ready? [Y/n] ").trim match {
            case "y" | "Y" => {

                val csv = new CsvReader(csvFilePath)
                val data = csv.toArray
                val dataGen = DataGenerator(data, startTime, endTime)

                val algo =
                algoName.toLowerCase match {
                    case "mth3" => {
                        new ManticoreHeur3(dataGen)
                    }
                    case "frac1" => new Fractal1(dataGen)
                }

                val tester = new AlgoTester(dataGen, algo)
                val result = tester.play()

                result.printSummary()

                csv.close()
            }
            case _ =>
                println("aborted.")
        }
    }
}


