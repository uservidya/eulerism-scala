package ru.smartislav

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.specification.AllExpectations


@RunWith(classOf[JUnitRunner])
abstract class SpecBase extends Specification with ScalaCheck with AllExpectations {
}
