/******************************************************************************
  *
  * A Chromosome is an indexed & mutable sequence of genes
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

trait Chromosome[Gene] extends collection.mutable.IndexedSeq[Gene]