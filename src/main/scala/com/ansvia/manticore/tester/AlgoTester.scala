package com.ansvia.manticore.tester

import com.ansvia.manticore._
import com.ansvia.manticore.algo._
import com.ansvia.manticore.algo.Ignored
import com.ansvia.manticore.DataGenerator
import org.streum.configrity.Configuration

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 5:41 PM
 *
 */

object TestingMode {
    val CANDLE = 1
    val ZZLEG = 2
}

class AlgoTester(dataGen:DataGenerator, algo:ManticoreAlgo,
                 startTime:String="", endTime:String="", mode:Int,
                 config:Configuration) {

    val debugMode = config("debugMode",false)

    case class TesterResult(var passed:Int, var missed:Int, allLegSuccess:Seq[Double]){
        def printSummary() = {
            val modeStr = mode match {
                case TestingMode.CANDLE => "candle"
                case TestingMode.ZZLEG => "zzleg"
            }
            val accuracy = (passed * 100).toDouble / (passed + missed).toDouble
            val legSuccessAvg = allLegSuccess.filter(!_.isNaN).sum / allLegSuccess.length.toDouble
            println(
                """
                  |Summary:
                  |-----------
                  |   Mode: %s
                  |   Data size: %d
                  |   Start time: %s
                  |   End time: %s
                  |   Result: %d passed and %d missed.
                  |   Success rate: %.02f %%
                  |   Accuracy level: %.02f %%
                """.stripMargin.format(modeStr, dataGen.data.size, dataGen.startTime, dataGen.endTime,
                    passed, missed, legSuccessAvg, accuracy)
            )
        }

    }


    // test starting point
    private val startTs = if (startTime.length > 0)
        Util.parseTime(startTime).getTime
    else
        dataGen.data(0).timestamp

    // test end point
    private val endTs = if (endTime.length > 0)
        Util.parseTime(endTime).getTime
    else
        dataGen.data(dataGen.data.size - 1).timestamp

    lazy val chunkedData = {
        if (startTime.length > 0 && endTime.length > 0){
            dataGen.data.filter { d =>
                d.timestamp > startTs && d.timestamp < endTs
            }
        }else if (startTime.length > 0 && endTime.length == 0){
            dataGen.data.filter { d =>
                d.timestamp > startTs
            }
        }else if (startTime.length == 0 && endTime.length > 0){
            dataGen.data.filter { d =>
                d.timestamp < endTs
            }
        }else{
            dataGen.data
        }
    }

    lazy val zzfChunked = new ZigZagFinder(chunkedData, 13, 8, 5).process()
    lazy val zzLegsChunked = zzfChunked.getLegs
    
    lazy val legIterator = zzLegsChunked.toIterator //dataGen.zzLegsChunked.toIterator


    algo match {
        case ai:AI => {
            ai.preTrain()
        }
        case _ =>
    }

    def play(slow:Boolean=false):TesterResult = {

        var curPos = 0

        var passes = 0
        var misses = 0
        var allLegSuccess = Seq.newBuilder[Double]
        var lastTimePos:Long = 0L
        var lastLeg:Leg = null

        println("testing...")

        while(legIterator.hasNext){

            val leg = legIterator.next()

            lastTimePos = leg.timestamp
            lastLeg = leg

//            if (leg.time == "2013.11.21 20:25"){
//                println("break")
//            }

//            // fix offset
//            while (leg.timestamp < dataGen.chunkedData(curPos).timestamp){
//                curPos = curPos + 1
//            }

//            curPos = curPos + leg.barCount

            var legPass = 0
            var legMiss = 0

            print(" > leg[%s] %02d".format(leg.time, leg.length))
            print(" " + (if (leg.direction==Direction.UP) "up" else if (leg.direction==Direction.DOWN) "dn" else "-") + ": ")

            for (i <- 0 to leg.barCount - 1){
                val timePos = chunkedData(curPos + i).time

                val result = try {
                    algo.calculate(timePos)
                }catch {
                    case e:Ignored => algo.lastResult
                }

                val direction = result.direction

                val origDirection = mode match {
                    case TestingMode.CANDLE => leg.barPattern(i).toInt
                    case TestingMode.ZZLEG => leg.direction
                }

                if (direction == origDirection){
                    passes = passes + 1
                    legPass = legPass + 1
                    print(".")
                }else if (direction == Direction.NEUTRAL){
                    print("_")
                }else{
                    misses = misses + 1
                    legMiss = legMiss + 1
                    print("x")

//                    if (direction == leg.barPattern(i).toInt){
//
//                        passes = passes + 1
//                        legPass = legPass + 1
//
//                        print(".")
//                    }else{
//
//                        misses = misses + 1
//                        legMiss = legMiss + 1
//
//                        print("x")
//
//                    }

                    // auto-correct
                    if (config("autoCorrect", false)){
                        algo match {
                            case ai:AI =>
                                ai.correctPrevious(result)
                            case _ =>
                        }
                    }

                }

                if (slow) Thread.sleep(500)
            }
//            if (legMiss > 30){

            var legSuccessPercent = (legPass * 100).toDouble / (legPass+legMiss).toDouble
            if (legSuccessPercent.isNaN)
                legSuccessPercent = 0.0

            if (legSuccessPercent < 60.0){
                //                    // train the algo if algo support AI
                algo match {
                    case ai:AI => {
                        val result = Result(leg.direction, 0.0)
//                        ai.correctPrevious(result)
                        ai.train(leg.barPattern.mkString("").grouped(4).mkString(" "), result)
                    }
                    case _ =>
                }
            }

            allLegSuccess += legSuccessPercent

            print("    %02d %02d/%02d %.02f%%".format(leg.length, legPass, legMiss, legSuccessPercent ))

//            if (debugMode){
//                print(" bc: %d, bch: %d, g: %d, b: %d".format(leg.length, leg.length/2, legPass, legMiss))
//            }

//            }
            println("")

            curPos = curPos + leg.barCount

            if (slow){
                Thread.sleep(5000)
            }
        }

        // rest bars
        val restCandles = chunkedData.filter(_.timestamp > lastTimePos)
        println(" -----------------------------------------")
        println(" > rest candles: " + restCandles.length)
        print  (" > prediction: ")
        val predicts =
        restCandles.map { candle =>
            val result = algo.calculate(candle.time)
            val direction = result.direction

            direction match {
                case Direction.UP => print(Console.BLUE + " U" + Console.RESET)
                case Direction.DOWN => print(Console.RED + " D" + Console.RESET)
                case Direction.UP => print("? ")
            }

            // auto correct
//            algo match {
//                case ai:AI =>
//                    ai.correctPrevious(result)
//                case _ =>
//            }

            direction
        }
        println("")

        if (predicts.length > 0){
            print(" > recommendation: ")
            predicts(predicts.length-1) match {
                case Direction.UP =>
                    if (lastLeg.direction == Direction.DOWN)
                        println("BUY")
                    else
                        println("HOLD")
                case Direction.DOWN =>
                    if (lastLeg.direction == Direction.UP)
                        println("SELL")
                    else
                        println("HOLD")
                case Direction.NEUTRAL => print("? ")
            }
        }
        println("")


        TesterResult(passes, misses, allLegSuccess.result())
    }


    def shutdown(){
        algo.close()
    }
}


object AlgoTester {

    val availableAlgos = Seq("MTH5", "FRAC1", "MTH6", "MTH7", "MTH8")

    def showUsage(){
        println("Usage: \n" +
            "AlgoTester [ALGO-NAME] [CSV-FILE] [START-TIME] [END-TIME]")
        sys.exit(2)
    }

    def main(args: Array[String]) {

        if (args.length < 3){
            showUsage()
        }

        val configFile = args(0)

        val conf = Configuration.load(configFile)
        

        val historyDataFile = conf[String]("manticore.history.data")
        val historyStartTime = conf[String]("manticore.history.range-start")
        val historyEndTime = conf[String]("manticore.history.range-end")

        val targetDataFile = if (args.length > 2) args(2) else ""

        var scanningStartTime:String = ""
        var scanningEndTime:String = ""

        var algoName:String = ""

        val interactive = args(1) == "--interactive"
        val debugMode = args.contains("--debug")
        val testingMode = args match {
            case x if x.contains("--candle-mode") => TestingMode.CANDLE
            case x if x.contains("--zzleg-mode") => TestingMode.ZZLEG
            case _ => TestingMode.ZZLEG
        }
        val slow = args.contains("--slow")

        val modeStr = testingMode match {
            case TestingMode.CANDLE => "candle"
            case TestingMode.ZZLEG => "zzleg"
        }

        val configTester = Configuration(
            "debugMode" -> debugMode,
            "autoCorrect" -> args.contains("--autocorrect")
        )


        if (interactive){
            println("Interactive mode")
            algoName = Console.readLine("algo name: ").trim
            scanningStartTime = Console.readLine("scanning start time: ").trim
        }else{
            algoName = args(1)
            scanningStartTime = if (args.length > 3) args(3) else ""
            scanningEndTime = if (args.length > 4 && !args(4).startsWith("--")) args(4) else ""
        }

        if (!availableAlgos.map(_.toLowerCase).contains(algoName.toLowerCase)){
            sys.error("No algo name: " + algoName)
            sys.error("Available algos: " + availableAlgos.mkString(", "))
            sys.exit(3)
        }

        println("Setup:")
        println("   algo name: " + algoName)
        println("   mode: " + modeStr)
        println("   source csv file: " + historyDataFile)
        println("   history start time: " + historyStartTime)
        println("   history end time: " + historyEndTime)
        println("   target csv file: " + targetDataFile)
        println("   scanning start time: " + scanningStartTime)
        println("   auto correct: " + configTester("autoCorrect", false))
        println("")

//        Console.readLine("ready? [Y/n] ").trim match {
//            case "y" | "Y" => {

                var done = false

                println("loading " + historyDataFile + "...")
                val csv = new CsvReader(historyDataFile)
                val data = csv.toArray
                val dataGenSource = DataGenerator(data, historyStartTime, historyEndTime)

                println("loading " + targetDataFile + "...")
                val csv2 = new CsvReader(targetDataFile)
                val data2 = csv2.toArray
                val dataGenTarget = DataGenerator(data2, "", "")

                val algo =
                algoName.toLowerCase match {
                    case "frac1" => new Fractal1(dataGenSource, dataGenTarget)
//                    case "mth3" => new ManticoreHeur3(dataGen)
                    case "mth5" => new ManticoreHeur5(dataGenSource, dataGenTarget)
                    case "mth6" => new ManticoreHeur6(dataGenSource, dataGenTarget, debugMode)
                    case "mth7" => new ManticoreHeur7(dataGenSource, dataGenTarget, debugMode)
                    case "mth8" => new ManticoreHeur8(dataGenSource, dataGenTarget, debugMode)
                }

                while(!done){

                    val tester = new AlgoTester(dataGenTarget, algo, scanningStartTime, scanningEndTime,
                        mode = testingMode,
                        configTester)
                    val result = tester.play(slow)

                    result.printSummary()

                    if (interactive){
                        val rv = Console.readLine("scanning start time: [" + scanningStartTime + "] ").trim
                        if (rv != "")
                            scanningStartTime = rv

                        if (rv == "x")
                            done = true
                    }else{
                        done = true
                    }

                    if (done){
                        tester.shutdown()
                    }
                }


                csv.close()
//            }
//            case _ =>
//                println("aborted.")
//        }
    }
}


