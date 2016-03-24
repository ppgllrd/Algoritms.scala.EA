/******************************************************************************
  *
  * Population of individuals. It's kept sorted in ascending order wrt
  * fitness of individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA


trait Population[Gene] {
  val size : Int // number of individuals

  protected val ea : EA[Gene] // access to EA where this population is used

  protected val individuals : Array[Individual[Gene]]

  def apply(idx : Int) : Individual[Gene] =
    individuals(idx)

  val worstIdx : Int = 0
  def worst() : Individual[Gene] = individuals(worstIdx)

  val bestIdx : Int = size-1
  def best() : Individual[Gene] = individuals(bestIdx)

  def sort(): Unit = {
    // sorted in ascending order wrt fitness
    scala.util.Sorting.quickSort(individuals)(Ordering by (_.fitness))
  }

  def initialize(eaState : EAState[Gene]) {
    for (i <- 0 until size)
      ea.initialize(individuals(i), i, eaState)
    sort()
  }

  // replace an individual and keep resulting population sorted
  def replace(idx : Int, ind : Individual[Gene]): Unit = {
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


case class StandardPopulation[Gene : Manifest](size : Int, ea : EA[Gene]) extends Population[Gene] {
  protected val individuals: Array[Individual[Gene]] =
    Array.fill[Individual[Gene]](size)(new Individual[Gene](ea.problem.numVars))
}

// Repeated individuals are not allowed
case class NonRepeatedPopulation[Gene : Manifest](size : Int, ea : EA[Gene]) extends Population[Gene] {
  protected val individuals: Array[Individual[Gene]] =
    Array.fill[Individual[Gene]](size)(new Individual[Gene](ea.problem.numVars))
  private def contains(ind : Individual[Gene], maxIdx : Int) : Boolean = {
    for(i <- 0 until maxIdx)
      if(ind==individuals(i))
        return true
    false
  }

  override def initialize(eaState : EAState[Gene]) {
    for(i <- 0 until size)
      do
        ea.initialize(individuals(i), i, eaState)
      while(contains(individuals(i), i))
    sort()
  }

  // replace an individual and keep resulting population sorted
  override def replace(idx : Int, ind : Individual[Gene]): Unit = {
    if(!contains(ind, size))
      super.replace(idx, ind)
  }
}
