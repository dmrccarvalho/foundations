package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.ExecutionContext

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature is consistent with List min (Oracle test)") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(_.temperatureFahrenheit))
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("size is consistent with List size") {
    forAll { (numbers: ParList[Sample]) =>
      assert(numbers.size == numbers.toList.size)
    }
  }

  test("averageTemperature PBT") {
    forAll { (samples: ParList[Sample]) =>
      averageTemperature(samples) match {
        case None => assert(samples.toList.isEmpty)
        case Some(avg) =>
          val newSamples = samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
          averageTemperature(newSamples) match {
            case None => fail("problem with map")
            case Some(avg2) =>
              assert((avg * 2 - avg2).abs < 0.00001) // Allow errors that may come when working with doubles
          }
      }

    }
  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }

  test("monoFoldLeft sum") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(Monoid.sumInt) == numbers.toList.sum)
    }
  }

  test("foldMap(identity) is consistent with monoFoldLeft") {
    forAll { (numbers: ParList[Int]) =>
      val monoid = Monoid.sumInt
      assert(numbers.map(identity).monoFoldLeft(monoid) == numbers.foldMap(identity)(monoid))
    }
  }

  test("parFoldMap is consistent with foldMap") {
    forAll { (numbers: ParList[Int]) =>
      val monoid = Monoid.sumInt
      assert(numbers.parFoldMap(identity)(monoid) == numbers.foldMap(identity)(monoid))
    }
  }

  test("monoFoldLeft is consistent with List foldLeft") {
    forAll { (numbers: ParList[Int], default: Int) =>
      assert(
        numbers.monoFoldLeft(Monoid.sumInt) == numbers.toList.foldLeft(Monoid.sumInt.default)(
          Monoid.sumInt.combine
        )
      )
    }
  }

  val doubleGen: Gen[Double] = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val intGen: Gen[Int]       = Gen.choose(Int.MinValue, Int.MaxValue)

  checkMonoid("sumInt", Monoid.sumInt, intGen)
  checkMonoid("sumDouble", Monoid.sumDouble, doubleGen)
  checkMonoid("zip", Monoid.zip(Monoid.sumInt, Monoid.sumInt), Gen.zip(intGen, intGen))
  checkMonoid("minSample", Monoid.minSample, Gen.option(sampleGen))
  checkMonoid("maxSample", Monoid.maxSample, Gen.option(sampleGen))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  def checkMonoid[A: Arbitrary](name: String, param: Monoid[A], gen: Gen[A]) = {
    test(s"checkMonoid $name - combine to be a no-op with default (identity)") {
      forAll { (value: A) =>
        assert(param.combine(value, param.default) == value)
        assert(param.combine(param.default, value) == value)
      }
    }

    test(s"checkMonoid $name - (associativity)") {
      forAll(gen, gen, gen) { (value1: A, value2: A, value3: A) =>
        val oneWay   = param.combine(param.combine(value1, value2), value3)
        val otherWay = param.combine(value1, param.combine(value2, value3))

        assert(oneWay == otherWay)
      }
    }
  }

}
