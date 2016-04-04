/******************************************************************************
  *
  * Individual with ArrayChromosome
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class ArrayIndividual[Gene : Manifest, Fitness](numVars : Int) extends Individual[Gene, Fitness] {
  val chromosome = new ArrayChromosome[Gene](numVars)

  // toDo avoid dynamic casting
  def copyFrom(that : Individual[Gene, Fitness]): Unit = that match {
    case that : ArrayIndividual[Gene, Fitness] =>
      this.chromosome.copyFrom(that.chromosome)
      this.fitness = that.fitness
    case _ =>
      sys.error("ArrayIndividual.copyFrom: ArrayIndividual expected")
    }

  override def equals(that : Any): Boolean = that match {
    case ind : ArrayIndividual[Gene,Fitness] =>
      this.chromosome.sameGenes(ind.chromosome)
    case _                                   =>
      false
  }

  override def toString : String = {
    val fitnessFormat = Fitness.fitness2Format(fitness)
    val sb = new StringBuilder(("ArrayIndividual("+fitnessFormat+", ").format(fitness))
    for(g <- chromosome)
      sb.append(g)
    sb.append(")")
    sb.toString()
  }
}