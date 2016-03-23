/******************************************************************************
  *
  * Population of individuals. It's kept sorted in ascending order wrt
  * fitness of individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA


trait Population {
  val size : Int // number of individuals

  protected val ea : EA // access to EA where this population is used

  protected val individuals : Array[Individual] =
    Array.fill[Individual](size)(new Individual(ea.problem.numVars))

  def apply(idx : Int) : Individual =
    individuals(idx)

  val worstIdx : Int = 0
  def worst() : Individual = individuals(worstIdx)

  val bestIdx : Int = size-1
  def best() : Individual = individuals(bestIdx)

  def sort(): Unit = {
    // sorted in ascending order wrt fitness
    scala.util.Sorting.quickSort(individuals)(Ordering by (_.fitness))
  }

  def initialize(eaState : EAState) {
    for (i <- 0 until size)
      ea.initialize(individuals(i), i, eaState)
    sort()
  }

  // replace an individual and keep resulting population sorted
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


case class StandardPopulation(size : Int, ea : EA) extends Population

// Repeated individuals are not allowed
case class NonRepeatedPopulation(size : Int, ea : EA) extends Population {
  private def contains(ind : Individual, maxIdx : Int) : Boolean = {
    for(i <- 0 until maxIdx)
      if(ind==individuals(i))
        return true
    false
  }

  override def initialize(eaState : EAState) {
    for(i <- 0 until size)
      do
        ea.initialize(individuals(i), i, eaState)
      while(contains(individuals(i), i-1))
    sort()
  }

  // replace an individual and keep resulting population sorted
  override def replace(idx : Int, ind : Individual): Unit = {
    if (contains(ind, size))
      return
    else
      super.replace(idx, ind)
  }
}
