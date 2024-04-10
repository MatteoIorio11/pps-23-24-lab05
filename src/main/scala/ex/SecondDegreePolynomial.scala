package ex

import scala.annotation.targetName

// Express a second degree polynomial
// Structure: secondDegree * X^2 + firstDegree * X + constant
trait SecondDegreePolynomial:
  def constant: Double
  def firstDegree: Double
  def secondDegree: Double
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial
  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial

case class SecondDegreePolynomialImpl(val secondDegree: Double, val firstDegree: Double, val constant: Double) extends SecondDegreePolynomial:
  override def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    SecondDegreePolynomial.createSecondDegreeFromOperation(this, polynomial)((x1, x2) => x1 - x2)


  override def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial =
    SecondDegreePolynomial.createSecondDegreeFromOperation(this, polynomial)((x1, x2) => x1 + x2)

object SecondDegreePolynomial:
  def apply(secondDegree: Double, firstDegree: Double, constant: Double): SecondDegreePolynomial = SecondDegreePolynomialImpl(secondDegree, firstDegree, constant)
  def createSecondDegreeFromOperation(pl1: SecondDegreePolynomial, pl2: SecondDegreePolynomial)(f: (Double, Double) => Double): SecondDegreePolynomial =
    SecondDegreePolynomialImpl(f(pl1.secondDegree, pl2.secondDegree),
      f(pl1.firstDegree, pl2.firstDegree),
      f(pl1.constant, pl2.constant))

@main def checkComplex(): Unit =
  val simplePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)
  val sum = simplePolynomial + anotherPolynomial
  println((sum, sum.secondDegree, sum.firstDegree, sum.constant)) // 1.0 * X^2 + 1.0 * X + 3.0
  val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
  println((multipleOperations, multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant)) // 2.0 * X^2 + 1.0 * X + 2.0

/** Hints:
  *   - implement SecondDegreePolynomial with a SecondDegreePolynomialImpl class, similar to PersonImpl in slides
  *   - check that equality and toString do not work
  *   - use a case class SecondDegreePolynomialImpl instead
  *   - check equality and toString now
  */
