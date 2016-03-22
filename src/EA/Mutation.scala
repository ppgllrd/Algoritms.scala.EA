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

  def flipBit(ind : Individual, rnd : Random, probMut : Probability): Unit = {
    for(i <- 0 until ind.numVars)
      if(rnd.nextDouble() < probMut)
        ind(i) = flip(ind(i))
  }
}


