/******************************************************************************
  *
  * Mutation operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object BinaryMutation {
  def flip(b : Bit) : Bit = (1-b).toByte

  def flipBit(ind : Individual[Bit], mutProb : Probability, rnd : Random): Unit = {
    for(i <- 0 until ind.chromosome.size)
      if(rnd.nextDouble() < mutProb)
        ind.chromosome(i) = flip(ind.chromosome(i))
  }
}


