/******************************************************************************
  *
  * Replacement operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object Replacement {
  def worst(pop : Population, ind : Individual): Unit = {
    pop.replace(pop.worstIdx, ind)
  }

  def random(pop : Population, ind : Individual, rnd : Random): Unit = {
    pop.replace(rnd.nextInt(pop.size), ind)
  }

  def best(pop : Population, ind : Individual): Unit = {
    pop.replace(pop.bestIdx, ind)
  }
}
