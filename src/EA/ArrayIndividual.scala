/******************************************************************************
  *
  * Individual with ArrayChromosome
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/
package EA

case class ArrayIndividual[Gene : Manifest](numVars : Int) extends Individual[Gene] {
  val chromosome : Chromosome[Gene] = new ArrayChromosome[Gene](numVars)

  def copyFrom(that : Individual[Gene]): Unit = {
    Array.copy(that.chromosome, 0, this.chromosome, 0, numVars)
    this.fitness = that.fitness
  }

  override def equals(that : Any): Boolean = that match {
    case ind : Individual[Gene] => this.chromosome.sameElements(ind.chromosome)
    case _                      => false
  }

  override def toString : String = {
    val sb = new StringBuilder("ArrayIndividual(%.5f, ".format(fitness))
    for(g <- chromosome)
      sb.append(g)
    sb.append(")")
    sb.toString()
  }
}