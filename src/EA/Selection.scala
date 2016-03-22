/******************************************************************************
  *
  * Selection operators
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

object Selection {
  def binaryTournament(pop : Population, rnd : Random): Individual = {
    val idx1 = rnd.nextInt(pop.size)
    var idx2 = 0
    do {
      idx2 = rnd.nextInt(pop.size)
    } while(idx1 == idx2)

    val ind1 = pop(idx1)
    val ind2 = pop(idx2)
    if(ind1.fitness > ind2.fitness)
      ind1
    else
      ind2
  }

  def random(pop : Population, rnd : Random) : Individual =
    pop(rnd.nextInt(pop.size))
}
