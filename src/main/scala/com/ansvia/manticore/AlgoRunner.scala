package com.ansvia.manticore

import com.ansvia.manticore.algo._
import org.streum.configrity.Configuration
import java.text.SimpleDateFormat

/**
 * Author: robin
 * Date: 12/1/13
 * Time: 5:41 PM
 *
 */


object AlgoRunner {

    val availableAlgos = Seq("MTH3", "MTH5", "FRAC1")

    def showUsage(){
        println("Usage: \n" +
            "AlgoRunner [CONFIG-FILE] [OPT]")
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
        var algoName:String = ""

        println("Interactive mode")
        algoName = Console.readLine("algo name: ").trim
        scanningStartTime = Console.readLine("scanning start time: ").trim

        if (!availableAlgos.map(_.toLowerCase).contains(algoName.toLowerCase)){
            sys.error("No algo name: " + algoName)
            sys.error("Available algos: " + availableAlgos.mkString(", "))
            sys.exit(3)
        }

        println("Setup:")
        println("   algo name: " + algoName)
        println("   source csv file: " + historyDataFile)
        println("   history start time: " + historyStartTime)
        println("   history end time: " + historyEndTime)
        println("   target csv file: " + targetDataFile)
        println("   scanning start time: " + scanningStartTime)
        println("")


        Console.readLine("ready? [Y/n] ").trim match {
            case "y" | "Y" => {

                var done = false

                println("loading " + historyDataFile + "...")
                val csv = new CsvReader(historyDataFile)
                val data = csv.toArray
                val dataGen = DataGenerator(data, historyStartTime, historyEndTime)

                println("loading " + targetDataFile + "...")
                var csv2 = new CsvReader(targetDataFile)
                val data2 = csv2.toArray
                var dataGenTarget = DataGenerator(data2, "", "")


                val algo =
                    algoName.toLowerCase match {
    //                    case "mth3" => new ManticoreHeur3(dataGen)
                        case "mth5" => new ManticoreHeur5(dataGen, dataGenTarget)
    //                    case "frac1" => new Fractal1(dataGen)
                    }

                while(!done){

                    val scStartTime = Util.parseTime(scanningStartTime).getTime
                    val candles = dataGenTarget.data.filter(_.timestamp > scStartTime)

                    var prevs = Seq.empty[String]
                    var dominator = ""

                    println("Probability: ")
                    candles.foreach { candle =>
                        val direction = algo.calculate(candle.time).direction
                        val directionStr = direction match {
                            case Direction.UP => "UP"
                            case Direction.DOWN => "DOWN"
                            case _ => "-"
                        }

                        dominator = prevs.find(x => prevs.count(_ == x) > prevs.length / 2).getOrElse("-")

                        if (directionStr == dominator){
                            println(candle.time + " -> " + directionStr)
                        }else{
                            println(candle.time + " -> " + Console.RED + directionStr + Console.RESET)
                        }

                        prevs :+= directionStr
                    }

                    println("dominator: " + dominator)

                    val rv = Console.readLine("scanning start time: [" + scanningStartTime + "] ").trim
                    if (rv != "")
                        scanningStartTime = rv

                    if (rv == "x")
                        done = true

                    if (!done){
                        println("loading " + targetDataFile + "...")
                        csv2.close()
                        csv2 = new CsvReader(targetDataFile)
                        val data2 = csv2.toArray
                        dataGenTarget = DataGenerator(data2, "", "")
                    }
                }

                csv.close()
            }
            case _ =>
                println("aborted.")
        }
    }
}


