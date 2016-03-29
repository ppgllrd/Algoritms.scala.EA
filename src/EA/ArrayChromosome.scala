/******************************************************************************
  *
  * An implementation of a Chromosome by using an array
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

case class ArrayChromosome[Gene : Manifest](override val size : Int) extends Chromosome[Gene] {

  val length = size

  protected val xs = new Array[Gene](size)

  def apply(idx : Int) : Gene = xs(idx)

  def update(idx : Int, g : Gene) = xs(idx) = g

  override def toString : String = {
    val sb = new StringBuilder("ArrayChromosome(")
    for(i <- 0 until size-1)
      sb.append(xs(i)+",")
    if(size>0)
      sb.append(xs(size-1))
    sb.append(")")
    sb.toString()
  }
}
