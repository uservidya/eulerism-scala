package ru.smartislav

import org.scalacheck.Gen
import spire.math.Rational

package object poly {
  def rational(n: Gen[Long], d: Gen[Long]): Gen[Rational] = for {
    num <- n
    denom <- d
  } yield Rational(num, denom)

  def rational: Gen[Rational] = rational(Gen.chooseNum(-10, 10), Gen.choose(1, 10))

  def nonZeroRational: Gen[Rational] = rational(Gen.oneOf(Gen.chooseNum(-10l, -1l), Gen.chooseNum(1l, 10l)),
    Gen.choose(1l, 10l))

  def posRational: Gen[Rational] = rational(Gen.chooseNum(0, 10), Gen.choose(1, 10))

  def factors(min: Int = 0, max: Int = 6) = for {
    vsQty <- Gen.choose(min, max)
    vs <- Gen.pick(vsQty, Seq("u", "v", "w", "x", "y", "z"))
    powers <- Gen.listOfN(vsQty, Gen.choose(1, 10))
  } yield vs zip powers

  def monomial = for {
    c <- rational
    powers <- factors()
  } yield Monomial(c, powers: _*)

  def nonZeroMonomial = for {
    c <- nonZeroRational
    powers <- factors(min = 1)
  } yield Monomial(c, powers: _*)

  def polynomial = for {
    mQty <- Gen.chooseNum(0, 3)
    ms <- Gen.listOfN(mQty, monomial)
  } yield Polynomial(ms: _*)

  def nonZeroPolynomial = for {
    mQty <- Gen.chooseNum(1, 3)
    ms <- Gen.listOfN(mQty, nonZeroMonomial)
  } yield Polynomial(ms: _*)
}
