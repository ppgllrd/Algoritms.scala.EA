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

  val heuristicOrder : Array[Int] = {
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

  private val zero : Byte = 0
  private val one : Byte = 1

  def repair(xs : Chromosome): Unit = {
    val sums = new Array[Double](numConstraints)
    for(i <- 0 until numConstraints)
      for(j <- 0 until numObjects)
        sums(i) += xs(j) * constraints(i)(j)

    val sums2 = sums.clone()

    // Drop variables until all constraints are satisfied
    var violatedConstraints = collection.mutable.Set[Int]()
    for(i <- 0 until numConstraints)
      if(sums(i) > capacities(i))
        violatedConstraints += i

    var it = heuristicOrder.reverseIterator
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

    // Add variables (in reverese order) while all constraints are satisfied
    it = heuristicOrder.iterator
    var add = true
    while(add && it.hasNext) {
      val j = it.next()
      if(xs(j)<one) {
        for (i <- 0 until numConstraints) {
          sums2(i) += constraints(i)(j)
          if (sums2(i) > capacities(i))
            add = false
        }
        if(add)
          xs(j) = one
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


object Test extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  val p = MKProblem.fromFile("data/MKP/OR10x250-00.kp1")

  val ea = new StandardSteadyStateTimedEA(seed = 1, problem = p, maxRunTime = 1000) {
/*
    override def recombine(child : Individual, parent1 : Individual, parent2 : Individual, eaState : EAState): Unit = {
      Recombination.singlePoint(child, parent1, parent2, eaState.rnd)
    }
    */
    override def replace(ind : Individual, eaState : EAState) {
      Replacement.randomButBest(5,eaState.population, ind, eaState.rnd)
    }
  }

  val result = ea.run()

  print("Final solution: "+result.best)
}