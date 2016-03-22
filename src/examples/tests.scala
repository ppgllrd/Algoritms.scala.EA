import java.util.{Locale, Random}


/*******************************************************************************
  *
  * Clase para medir tiempos de ejecución de un programa
  *
  * Autor:
  *
  *****************************************************************************/






/*
object Ejemplo extends App {
  import java.util.Random

  val t = Timer()

  t.reset()

  val seed = 0
  val rnd = new Random(seed)
  var x = 0
  for(i <- 1 to 10000000)
    x += rnd.nextInt(1000)

  val segs = t.elapsedTime()

  println("Tiempo que ha tardado = %.2f segundos".format(segs))
}






object SimplexTest extends App {
  // Maximizar:
  // f = 0.5 x3 + 3 x2 + 1 x1 + 4 x0
  val c = Array[Double](0.5,3,1,4)

  // Sujeto a:
  // 1 x3 + 1 x2 + 1 x1 + 1 x0 <= 40
  // 2 x3 + 1 x2 - 1 x1 - 1 x0 <= 10
  //      - 1 x2        + 1 x0 <= 10

  val b = Array[Double](40, 10, 10)

  val A : Array[Array[Double]] =
    Array( Array(1,1,1,1)
      , Array(2,1,-1,-1)
      , Array(0,-1,0,1)
    )

  val pl = LinearRelaxation(c,A,b)
  pl.solve()

  println(pl.optimalValue())
  for(x <- pl.primalSol())
    print(x+" ")
  println()
  for(x <- pl.dualSol())
    print(x+" ")

  /*
  145.0
0.0 15.0 0.0 25.0
3.5 -0.0 0.5
   */
}


object TestMochila extends App {
  // Maximizar:
  // f = 0.5 x3 + 3 x2 + 1 x1 + 4 x0
  val c = Array[Double](0.5,3,1,4)

  // Sujeto a:
  // 1 x3 + 1 x2 + 1 x1 + 1 x0 <= 40
  // 2 x3 + 1 x2 + 1 x1 + 1 x0 <= 10
  //      + 1 x2        + 1 x0 <= 10

  val b = Array[Double](40, 10, 10)

  val A : Array[Array[Double]] =
    Array( Array(1,2,1,5)
      , Array(2,1,1,1)
      , Array(0,1,0,1)
    )

  val p = MMKProblem(c,A,b)
}










*/









/*
object examples.MKP.Simplex extends App {
  import org.apache.commons.math3.optim.MaxIter
  import org.apache.commons.math3.optim.linear._
  import org.apache.commons.math3.optim.nonlinear.scalar.GoalType

  // Maximizar:
  // f = 0.5 x3 + 3 x2 + 1 x1 + 4 x0 + 0
  val f = new LinearObjectiveFunction(Array[Double](0.5,3,1,4), 0)

  // Sujeto a:
  // 1 x3 + 1 x2 + 1 x1 + 1 x0 <= 40
  // 2 x3 + 1 x2 - 1 x1 - 1 x0 >= 10
  //      - 1 x2        + 1 x0 >= 10
  val constraints = new java.util.ArrayList[LinearConstraint]()
  constraints.add(new LinearConstraint(Array[Double](1,1,1,1), Relationship.LEQ, 40))
  constraints.add(new LinearConstraint(Array[Double](2,1,-1,-1), Relationship.GEQ, 10))
  constraints.add(new LinearConstraint(Array[Double](0,-1,0,1), Relationship.GEQ, 10))
  val constraintSet = new LinearConstraintSet(constraints)

  val simplexSolver = new SimplexSolver()

  val solution = simplexSolver.optimize(new MaxIter(1000), f, constraintSet, GoalType.MAXIMIZE, new NonNegativeConstraint(true))

  val points : Array[Double] = solution.getPoint // Array con los valores de cada variable

  val valorMáximo = f.value(points) // Valor de f para la solución

  println("El valor máximo de la función es %f".format(valorMáximo))
  println("El punto para esta solución es:")
  for(i <- points.indices)
    print("x%d = %f ".format(i, points(i)))
}
*/