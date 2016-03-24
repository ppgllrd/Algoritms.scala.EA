/******************************************************************************
  *
  * Individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class Individual[Gene : Manifest](numVars : Int) {
  val chromosome : Chromosome[Gene] = new Array[Gene](numVars)
  var fitness : Fitness = 0

  def apply(idx : Int) : Gene = chromosome(idx)

  def update(idx : Int, gen : Gene): Unit = {
    chromosome(idx) = gen
  }

  def copyFrom(that : Individual[Gene]): Unit = {
    Array.copy(that.chromosome, 0, this.chromosome, 0, numVars)
    this.fitness = that.fitness
  }

  override def equals(that : Any): Boolean = that match {
    case ind : Individual[Gene] => this.chromosome.sameElements(ind.chromosome)
    case _                      => false
  }

  override def toString : String = {
    val sb = new StringBuilder("Individual(%.5f, ".format(fitness))
    for(g <- chromosome)
      sb.append(g)
    sb.append(")")
    sb.toString()
  }
}