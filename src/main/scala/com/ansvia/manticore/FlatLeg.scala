package com.ansvia.manticore

import scala.collection.mutable

/**
 * Author: robin
 * Date: 11/11/13
 * Time: 11:04 PM
 *
 */
object FlatLeg {

    def main(args: Array[String]) {

        val untilDate = {
            if (args.length > 1)
                args(1)
            else
                "-"
        }


        val data = new CsvReader(args(0), untilDate).toArray.toIndexedSeq

        val zz = new ZigzagFinder(data)

        //             zz.process().getZigZagBuffer.zipWithIndex.foreach { case (z, i) =>
        //                 println("%d. %s => %s".format(i, data(i).time, z))
        //             }

        val legs = zz.getLegs

        //             var set2: immutable.IndexedSeq[Seq[Seq[Int]]] = immutable.IndexedSeq[Seq[Seq[Int]]]()
        var set2a = new mutable.HashMap[Int,Seq[Seq[Int]]]

        legs //.filter(leg => leg.fractalCount > 3 && leg.fractalCount < 14)
            .zipWithIndex.foreach { case (d, i) =>

            println("%d. %s".format(i, d))

            //                 set2 ++=
            for(n <- 4 to 13){
                val aoeu = Manticore.getDnas(new InlineDataSource(d.fractalPattern.map(_.toInt).toSeq), n)
                    .map(dd => dd.map(_._1))
                if (set2a.contains(n)){
                    val aoeu2 = set2a.get(n).get ++ aoeu
                    set2a.update(n, aoeu2)
                }else{
                    set2a += n -> aoeu
                }
            }


            //                 val result = dnas.map( dna => Manticore.breakDown(dna, data) )

        }

        println("")

        var back = 1
        val legUsed = legs(legs.length - back)
        println("leg used to be pattern: " + legUsed)

        var finalPattern = legUsed.fractalPattern
        back = back + 1
        var legCount = 1
        while(finalPattern.length < 3){
            legCount = legCount + 1
            val leg2 = legs(legs.length - back)
            finalPattern = leg2.fractalPattern ++ finalPattern
            back = back + 1
        }

        println("using %d leg(s) as pattern".format(legCount))

        val pattBase: Seq[Int] = finalPattern.map(_.toInt).toSeq
        val pattUp = finalPattern.map(_.toInt).toSeq ++ Seq(1)
        val pattDown = finalPattern.map(_.toInt).toSeq ++ Seq(0)

        println("\n")
        println("base pattern: {"+ pattBase.mkString(",") + "}\n")
        println("up pattern: {" + pattUp.mkString(",") + "}")
        println("down pattern: {" + pattDown.mkString(",") + "}")

        var stats = new mutable.HashMap[String, (Int, Leg)]()

        println("Searching for pattern...")
        var patternCount = 0
        //             for ( patterns <- set2a.values ){

        val legsCount = legs.length

        //                 patterns.foreach { patt =>
        legs.zipWithIndex.foreach { case (leg, i) =>
            val patt = leg.fractalPattern.toSeq.map(_.toInt)
            if (patt/*.map(_._1)*/.startsWith(pattBase)){
                val pattStr = patt.mkString(",")
                if (patternCount < 20){
                    println("   + found: {" + pattStr + "}")
                    if (patternCount == 19)
                        println("   + and more...")
                }
                patternCount = patternCount + 1
                val hleg = stats.get(pattStr)
                if (hleg.isDefined){
                    val vv = hleg.get
                    val count = vv._1 + 1
                    if (i < legsCount-1){
                        stats += pattStr ->  (count, legs(i+1))
                    }else{
                        stats += pattStr -> (count, null)
                    }
                }else{
                    if (i < legsCount-1){
                        stats += pattStr ->  (1, legs(i+1))
                    }else{
                        stats += pattStr -> (1, null)
                    }
                }
            }
        }

        //             }

        println("Statistics: ===")
        for ( (patt, leg) <- stats.toSeq.sortBy(_._2._1).reverse.slice(0,10) ){
            //                 if (patt != pattBase.mkString(",")){
            val count = leg._1
            val nextLeg = leg._2
            println(" %d \t- %s".format(count, patt))
            if (nextLeg != null){
                println("   \t   next-leg: " + nextLeg)
            }
            //                 }
        }

        println("\n")
    }
}
