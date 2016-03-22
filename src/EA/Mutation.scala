/******************************************************************************
  *
  * Mutation operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object Mutation {
  def flip(b : Bit) : Bit = (1-b).toByte

  def flipBit(ind : Individual, mutProb : Probability, rnd : Random): Unit = {
    for(i <- 0 until ind.numVars)
      if(rnd.nextDouble() < mutProb)
        ind(i) = flip(ind(i))
  }
}


