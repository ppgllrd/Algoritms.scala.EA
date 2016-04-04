/******************************************************************************
  *
  * One Max problem: maximize number of 1s in a binary string
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package examples.oneMax

import java.util.Locale
import EA._
import EA.util.Logger

object OneMax {
  type Gene = Bit
  type Fitness = Int
}

import OneMax._

case class OneMax(numVars : Int) extends Problem[Gene, Fitness] {
  override def isOptimal(ind: Individual[Gene, Fitness]): Boolean =
    ind.fitness == numVars

  override def computeFitness(ind: Individual[Gene, Fitness]): Fitness =
    ind.chromosome.count(_==1)
}


object OneMaxEA extends App {
  // Use English formats
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  val numVars = 500 // number of variables

  val ea =
    StandardSteadyStateNonRepeatedPopTimedBinaryEA[Fitness]( seed = 0
                                                           , logger = Logger[Fitness]()
                                                           , problem = OneMax(numVars)
                                                           , maxRunTime = 1000
                                                           )

  val result = ea.run()

  println("Final solution: "+result.best)
  result.logger.print()
}

