package com.ansvia.manticore.tester

import com.ansvia.manticore._
import java.text.SimpleDateFormat
import com.ansvia.manticore.Record
import com.ansvia.manticore.algo.{Ignored, ManticoreHeur5, ManticoreAlgo}

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 5:41 PM
 *
 */
class AlgoTester(dataGen:DataGenerator, algo:ManticoreAlgo) {

    case class TesterResult(var passed:Int, var missed:Int){
        def printSummary() = {
            val accuracy = (passed * 100).toDouble / (passed + missed).toDouble
            println(
                """
                  |Summary:
                  |-----------
                  |   Data size: %d
                  |   Start time: %s
                  |   End time: %s
                  |   Result: %d passed and %d missed.
                  |   Accuracy level: %.02f %%
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

            if (leg.time == "2013.11.21 20:25"){
                println("break")
            }

//            // fix offset
//            while (leg.timestamp < dataGen.chunkedData(curPos).timestamp){
//                curPos = curPos + 1
//            }

//            curPos = curPos + leg.barCount

            var legMiss = 0

            print(" " + leg.directionStr + ": ")

            for (i <- 0 to leg.barCount - 1){
                val direction = try {
                    algo.calculate(curPos + i).direction
                }catch {
                    case e:Ignored => algo.lastResult.direction
                }

                if (direction == leg.direction || (direction!=leg.direction && i==leg.barCount-1)){
                    passes = passes + 1
                    print(".")
                }else if (direction == Direction.NEUTRAL){
                    print("_")
                }else{
                    misses = misses + 1
                    legMiss = legMiss + 1
                    print("x")
                }
            }
//            if (legMiss > 30){
                print(" -> leg[%s]".format(leg.time))
//            }
            println("")

            curPos = curPos + leg.barCount

        }

        TesterResult(passes, misses)
    }


}


object AlgoTester {

    val availableAlgos = Seq("MTH3", "MTH5", "FRAC1")

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
//        Console.readLine("ready? [Y/n] ").trim match {
//            case "y" | "Y" => {

                val csv = new CsvReader(csvFilePath)
                val data = csv.toArray
                val dataGen = DataGenerator(data, startTime, endTime)

                val algo =
                algoName.toLowerCase match {
                    case "mth3" => new ManticoreHeur3(dataGen)
                    case "mth5" => new ManticoreHeur5(dataGen)
                    case "frac1" => new Fractal1(dataGen)
                }

                val tester = new AlgoTester(dataGen, algo)
                val result = tester.play()

                result.printSummary()

                csv.close()
//            }
//            case _ =>
//                println("aborted.")
//        }
    }
}


