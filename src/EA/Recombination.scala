/******************************************************************************
  *
  * Recombination operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object Recombination {
  def uniform[Gene](child : Individual[Gene], parent1 : Individual[Gene], parent2 : Individual[Gene], rnd : Random): Unit = {
    for(i <- 0 until child.chromosome.size)
      child.chromosome(i) = (if(rnd.nextDouble() < 0.5) parent1 else parent2).chromosome(i)
  }

  def singlePoint[Gene](child : Individual[Gene], parent1 : Individual[Gene], parent2 : Individual[Gene], rnd : Random): Unit = {
    val p = rnd.nextInt(child.chromosome.size)

    Array.copy(parent1.chromosome, 0, child.chromosome, 0, p)
    Array.copy(parent2.chromosome, p, child.chromosome, p, child.chromosome.size-p)
  }
}