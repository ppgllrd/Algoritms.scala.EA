/******************************************************************************
  *
  * Initialization operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object Initialization {
  def random[Gene](ind : Individual[Gene], rndGen : () => Gene): Unit = {
    for(i <- 0 until ind.chromosome.size)
      ind.chromosome(i) = rndGen()
  }
}

object BinaryInitialization {
  def random(ind : Individual[Bit], rnd : Random): Unit = {
    for(i <- 0 until ind.chromosome.size)
      ind.chromosome(i) = rnd.nextInt(2).toByte
  }
}