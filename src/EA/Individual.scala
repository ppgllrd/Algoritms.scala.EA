/******************************************************************************
  *
  * Individuals
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class Individual(val numVars : Int) {
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

  override def toString : String = {
    val sb = new StringBuilder("Individual(%.5f, ".format(fitness))
    for(g <- chromosome)
      sb.append(g)
    sb.append(")")
    sb.toString()
  }
}