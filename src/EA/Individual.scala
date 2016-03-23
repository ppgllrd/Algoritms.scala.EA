/******************************************************************************
  *
  * Individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class Individual(numVars : Int) {
  val chromosome : Chromosome = new Array[Gen](numVars)
  var fitness : Fitness = 0

  def apply(idx : Int) : Gen = chromosome(idx)

  def update(idx : Int, gen : Gen): Unit = {
    chromosome(idx) = gen
  }

  def copyFrom(that : Individual): Unit = {
    Array.copy(that.chromosome, 0, this.chromosome, 0, numVars)
    this.fitness = that.fitness
  }

  override def equals(that : Any): Boolean = that match {
    case ind : Individual => this.chromosome.sameElements(ind.chromosome)
    case _                => false
  }

  override def toString : String = {
    val sb = new StringBuilder("Individual(%.5f, ".format(fitness))
    for(g <- chromosome)
      sb.append(g)
    sb.append(")")
    sb.toString()
  }
}