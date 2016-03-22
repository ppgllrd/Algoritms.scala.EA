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
  def random(ind : Individual, rnd : Random): Unit = {
    for(i <- 0 until ind.numVars)
      ind(i) = rnd.nextInt(2).toByte
  }
}