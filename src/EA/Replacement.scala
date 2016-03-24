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
  def worst[Gene](pop : Population[Gene], ind : Individual[Gene]): Unit = {
    pop.replace(pop.worstIdx, ind)
  }

  def random[Gene](pop : Population[Gene], ind : Individual[Gene], rnd : Random): Unit = {
    pop.replace(rnd.nextInt(pop.size), ind)
  }

  def randomButBest[Gene](pop : Population[Gene], ind : Individual[Gene], rnd : Random): Unit = {
    pop.replace(rnd.nextInt(pop.size-1), ind)
  }

  def randomButBest[Gene](n : Int, pop : Population[Gene], ind : Individual[Gene], rnd : Random): Unit = {
    pop.replace(rnd.nextInt(pop.size-n), ind)
  }

  def best[Gene](pop : Population[Gene], ind : Individual[Gene]): Unit = {
    pop.replace(pop.bestIdx, ind)
  }
}
