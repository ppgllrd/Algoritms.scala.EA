/******************************************************************************
  *
  * The Multidimensional Knapsack Problem
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package examples.MKP

import EA._


// max cx : Ax <= b, x >= 0 }.
// Assumes that b >= 0 so that x = 0 is a basic feasible solution.
case class LinearRelaxation(c : Array[Double], A : Array[Array[Double]], b : Array[Double]) {
  private val numVars = c.length
  private val numConstraints = A.length

  require(A.forall(_.length == numVars), "LinearRelaxation: number of columns in A must be equal to number of variables")
  require(A.length == b.length, "LinearRelaxation: number of constraints must be equal to length of b")

  private var opt : Option[Simplex] = None

  def solve(): Unit = {
    opt = Some(new Simplex(A,b,c))
  }

  def primalSol() : Array[Double] = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.primal()
  }

  def dualSol() : Array[Double] = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.dual()
  }

  def optimalValue() : Double = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.value()
  }
}

class MKProblem( val profits : Array[Double]
                , val constraints : Array[Array[Double]]
                , val capacities : Array[Double]
                , val optimum : Fitness = 0
                ) extends Problem {

  val numObjects : Int = profits.length

  val numConstraints : Int = constraints.length

  // Check problem preconditions
  require(capacities.length == numConstraints
    ,"MKProblem: Number of constraints must be the same as number of capacities"
  )
  require(profits.forall(_ > 0)
    ,"MKProblem: All profits must be > 0"
  )
  require(capacities.forall(_ >= 0)
    ,"MKProblem: All capacities must be >= 0"
  )
  require(constraints.forall(_.length == numObjects)
    ,"MKProblem: Each constraint must have same number of variables as number of objects"
  )
  require(constraints.forall(_.forall(_ >= 0))
    ,"MKProblem: Each constraint coefficient must be >= 0"
  )


  // Evals objective function for an assignments of vars
  def totalProfit(xs : Chromosome) : Fitness = {
    // require(xs.length == numObjects, "MKProblem.evaluate: Number of variables must be the same as number of objects")
    var s = 0.0
    for(j <- 0 until numObjects)
      s += profits(j) * xs(j)
    s
  }

  // Checks if an assignment satisfies all constraints
  def checkConstraints(xs : Chromosome) : Boolean = {
    for(i <- 0 until numConstraints) {
      var s = 0.0
      for(j <- 0 until numObjects)
        s += xs(j) * constraints(i)(j)
      if(s > capacities(i))
        return false
    }
    true
  }

  protected val heuristicOrder : Array[Int] = Array.range(0, numObjects)

  protected val zero : Byte = 0
  protected val one : Byte = 1

  def repair(xs : Chromosome): Unit = {
    val sums = new Array[Double](numConstraints)
    for(i <- 0 until numConstraints)
      for(j <- 0 until numObjects)
        sums(i) += xs(j) * constraints(i)(j)

    // Drop variables until all constraints are satisfied
    var violatedConstraints = collection.mutable.Set[Int]()
    for(i <- 0 until numConstraints)
      if(sums(i) > capacities(i))
        violatedConstraints += i

    val it = heuristicOrder.reverseIterator
    while(violatedConstraints.nonEmpty && it.hasNext) {
      val j = it.next()
      if(xs(j)>zero) {
        xs(j) = zero
        for (i <- violatedConstraints.clone()) {
          sums(i) -= constraints(i)(j)
          if(sums(i) <= capacities(i))
            violatedConstraints -= i
        }
      }
    }

    // Add variables (in reverse order) while all constraints are satisfied
    for(i <- 0 until numConstraints) {
      sums(i) = 0
      for (j <- 0 until numObjects)
        sums(i) += xs(j) * constraints(i)(j)
    }

    for(j <- heuristicOrder) {
      if(xs(j)<one) {
        var add = true
        var i = 0
        while(add && i < numConstraints) {
          sums(i) += constraints(i)(j)
          if(sums(i) > capacities(i))
            add = false
          else
            i += 1
        }
        if(add)
          xs(j) = one
        else  // undo
          for(k <- 0 to i)
            sums(k) -= constraints(k)(j)
      }
    }
  }

  val numVars = numObjects

  def computeFitness(ind : Individual) : Fitness = {
    repair(ind.chromosome)
    /*
    if(!checkConstraints(xs))
      sys.error("checkConstraints failed!")
    */
    totalProfit(ind.chromosome)
  }

  def isOptimal(ind : Individual) : Boolean =
    ind.fitness == optimum

  override def toString : String = {
    "toDo..." // toDo
  }
}

object MKProblem {
  def apply(values : Array[Double], constraints : Array[Array[Double]],  weights : Array[Double], optimum : Fitness = 0) =
    new MKProblem(values, constraints, weights, optimum)

  // Reads a problem from a text file
  def fromFile(fileName : String) : MKProblem = {
    import java.io.File
    import java.util.Scanner

    val file = new File(fileName)
    if(!file.exists())
      sys.error("File %s does not exist".format(fileName))

    val sc = new Scanner(file)

    val numObjects = sc.nextInt()
    val numConstraints = sc.nextInt()
    val optimum = sc.nextDouble()

    val values = new Array[Double](numObjects)
    for(j <- values.indices)
      values(j) = sc.nextDouble()

    val constraints = Array.ofDim[Double](numConstraints, numObjects)
    for(i <- constraints.indices)
      for(j <- constraints(i).indices)
        constraints(i)(j) = sc.nextDouble()

    val weights = new Array[Double](numConstraints)
    for(i <- weights.indices)
      weights(i) = sc.nextDouble()

    sc.close()

    new MKProblem(values, constraints, weights, optimum)
  }

  def apply(fileName : String) = fromFile(fileName)
}


class ChuBeasley( override val profits : Array[Double]
                 , override val constraints : Array[Array[Double]]
                 , override val capacities : Array[Double]
                 , override val optimum : Fitness = 0
                 ) extends MKProblem(profits,constraints,capacities,optimum) {

  override val heuristicOrder : Array[Int] = {
    // For linear relaxation, each variable x must be in: 0 <= x <= 1
    //    0 <= x is already part of problem, but we need to impose x <= 1
    val extraConstraints = Array.ofDim[Double](numObjects, numObjects)
    for (j <- 0 until numObjects)
      extraConstraints(j)(j) = 1

    val extraCapacities = Array.fill[Double](numObjects)(1)

    val linearRelaxation = LinearRelaxation(profits, constraints ++ extraConstraints, capacities ++ extraCapacities)
    linearRelaxation.solve()

    val a = linearRelaxation.dualSol()

    // compute pseudo-utility values
    val u = new Array[Double](numObjects)
    for(j <- 0 until numObjects) {
      var s = 0.0
      for(i <- 0 until numConstraints)
        s += a(i) * constraints(i)(j)
      u(j) = profits(j) / s
    }

    val ps = u.zip(Array.range(0,numObjects)) // u x order of variables
    scala.util.Sorting.quickSort(ps)(Ordering by (- _._1)) // sort in decreasing order wrt to u
    ps.map(_._2) // return order of variables
  }
}

object ChuBeasley {
  def fromFile(fileName : String) : ChuBeasley = {
    val mkp = MKProblem.fromFile(fileName)
    new ChuBeasley(mkp.profits, mkp.constraints, mkp.capacities, mkp.optimum)
  }
}

object ChuBeasleyEA extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

/*  val seed = 0
  val fileName = "data/MKP/OR10x100-00.kp1"
  val maxTime = 10
*/

  if(args.length < 3) {
    println("Usage: <seed> <file> <maxTime(seg.)>")
    System.exit(0)
  }
  val seed = args(0).toInt
  val fileName = args(1)
  val maxTime = args(2).toDouble


  val p = ChuBeasley.fromFile(fileName)

  val ea = new StandardSteadyStateNonRepeatedPopTimedEA(seed = seed, problem = p, maxRunTime = maxTime) {
    override def replace(ind : Individual, eaState : EAState) {
      Replacement.randomButBest(population.size/2, eaState.population, ind, eaState.rnd)
    }
  }

  val result = ea.run()
  if(!p.checkConstraints(result.best.chromosome)) {
    println("ERROR: non-valid solution!")
  } else {
    println("Final solution: " + result.best)
    println("EAResult: " + result)
  }
}