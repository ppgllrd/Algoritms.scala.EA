/******************************************************************************
  *
  * Evolutionary Algorithms
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

case class EAParams( val popSize : Int = 100
                    , val crossProb : Probability = 0.9
                    , val mutProb : Probability
                    , val maxRunTime : Seconds
                    ) {
  override def toString =
    "EAParams(popSize=%d, crossProb=%.3f, mutProb=%.3f, maxRunTime=%.3f segs.)".format(popSize,crossProb,mutProb,maxRunTime)
}


abstract class EA(seed : Int, params : EAParams, problem : Problem) {
  def initialize(ind : Individual, idx : Int, rnd : Random)

  def mutate(ind : Individual, rnd : Random, probMut : Probability)

  def select(pop : Population, rnd : Random) : Individual

  def recombine(child : Individual, parent1 : Individual, parent2 : Individual, rnd : Random)

  def replace(pop : Population, ind : Individual, rnd : Random)

  def run() : Individual
}


abstract class SteadyStateEA(seed : Int, params : EAParams, problem : Problem) extends EA(seed, params, problem) {
  override def run() : Individual = {
    val temp = util.Timer()
    temp.reset()

    val rnd = new Random(seed)

    println("Seed=%d".format(seed))
    println(params)

    // initialize population
    val population = Population(params.popSize, problem.numberVars)
    for (i <- 0 until params.popSize) {
      val ind = population(i)
      initialize(ind, i, rnd)
      ind.fitness = problem.evalSolution(ind.chromosome)
    }
    population.sort()

    val ind = Individual(problem.numberVars)

    val best = Individual(problem.numberVars)
    best.copyFrom(population.best())

    println("Fitness/Iter/Time")
    val format = "%10.2f\t%5d\t%10.2f"

    var iter = 0
    println(format.format(best.fitness, iter, temp.elapsedTime()))

    while(temp.elapsedTime() < params.maxRunTime && !problem.isOptimal(population.best().chromosome)) {
      iter += 1

      if(rnd.nextDouble() < params.crossProb) {
        val parent1 = select(population,rnd)
        val parent2 = select(population,rnd)

        recombine(ind, parent1, parent2, rnd)
      } else
        ind.copyFrom(Selection.random(population, rnd))

      mutate(ind, rnd, params.mutProb)

      ind.fitness = problem.evalSolution(ind.chromosome)

      if(ind.fitness > best.fitness) {
        best.copyFrom(ind)
        println(format.format(best.fitness, iter, temp.elapsedTime()))
      }

      replace(population, ind, rnd)
    }
    best
  }
}


case class StandardSteadyStateEA(seed : Int, problem : Problem, maxRunTime : Int) extends
            SteadyStateEA(seed, EAParams(popSize = 100, crossProb = 0.9, mutProb = 1.0/problem.numberVars, maxRunTime = maxRunTime), problem) {

  override def initialize(ind : Individual, idx : Int, rnd : Random) {
    Initialization.random(ind, rnd)
  }

  override def mutate(ind : Individual, rnd : Random, probMut : Probability) {
    Mutation.flipBit(ind, rnd, probMut)
  }

  override def select(pop : Population, rnd : Random) =
    Selection.binaryTournament(pop, rnd)

  def recombine(child : Individual, parent1 : Individual, parent2 : Individual, rnd : Random) =
    Recombination.uniform(child, parent1, parent2, rnd)

  def replace(pop : Population, ind : Individual, rnd : Random) {
    Replacement.worst(pop, ind)
  }
}