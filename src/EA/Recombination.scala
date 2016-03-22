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
  def uniform(child : Individual, parent1 : Individual, parent2 : Individual, rnd : Random): Unit = {
    for(i <- 0 until child.numVars)
      child(i) = (if(rnd.nextDouble() < 0.5) parent1 else parent2)(i)
  }

  def singlePoint(child : Individual, parent1 : Individual, parent2 : Individual, rnd : Random): Unit = {
    val p = rnd.nextInt(child.numVars)

    Array.copy(parent1.chromosome, 0, child.chromosome, 0, p)
    Array.copy(parent2.chromosome, p, child.chromosome, p, child.numVars-p)
  }
}