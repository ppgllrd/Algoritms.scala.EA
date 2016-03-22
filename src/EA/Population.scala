/******************************************************************************
  *
  * Population of individuals. It's kept sorted in ascending order wrt
  * fitness of individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class Population(val size : Int, numVars : Int) {
  private val individuals = Array.fill[Individual](size)(new Individual(numVars))

  def apply(idx : Int) = individuals(idx)

  def worst() : Individual = individuals(0)

  def best() : Individual = individuals(size-1)

  def sort(): Unit = {
    // sorted in ascending order wrt fitness
    scala.util.Sorting.quickSort(individuals)(Ordering by (_.fitness))
  }

  def replace(idx : Int, ind : Individual): Unit = {
    val toReplace = individuals(idx)
    var i = idx

    if(i > 0 && ind.fitness < individuals(i-1).fitness) {
      // float upwards
      do {
        individuals(i) = individuals(i-1)
        i -= 1
      } while(i > 0 && ind.fitness < individuals(i-1).fitness)
    } else if(i < size-1 && ind.fitness > individuals(i+1).fitness) {
      // push downwards
      do {
        individuals(i) = individuals(i+1)
        i += 1
      } while(i < size-1 && ind.fitness > individuals(i+1).fitness)
    }
    toReplace.copyFrom(ind)
    individuals(i) = toReplace
  }

  private def checkInOrder() {
    for(i <- 0 until size-1)
      if(individuals(i).fitness > individuals(i+1).fitness)
        sys.error("checkInOrder: failed. Individuals are not in order")
  }
}
